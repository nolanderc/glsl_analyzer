const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const util = @import("util.zig");
const lsp = @import("lsp.zig");
const rpc = @import("jsonrpc.zig");
const Response = rpc.Response;
const Request = rpc.Request;

const Workspace = @import("Workspace.zig");
const cli = @import("cli.zig");
const analysis = @import("analysis.zig");
const parse = @import("parse.zig");

pub const std_options: std.Options = .{
    .log_level = .debug,
};

fn enableDevelopmentMode(stderr_target: []const u8) !void {
    if (builtin.os.tag == .linux) {
        // redirect stderr to the build root
        const new_stderr = try std.posix.open(
            stderr_target,
            .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
            0o644,
        );
        try std.posix.dup2(new_stderr, std.posix.STDERR_FILENO);
        std.posix.close(new_stderr);
    } else {
        std.log.warn("development mode not available on {s}", .{@tagName(builtin.os.tag)});
        return error.UnsupportedPlatform;
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 8 }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var static_arena = std.heap.ArenaAllocator.init(allocator);
    defer static_arena.deinit();

    var alloc_args = try std.process.argsWithAllocator(allocator);
    defer alloc_args.deinit();

    const args = try cli.Arguments.parse(&alloc_args);

    if (args.dev_mode) |stderr_target| {
        if (enableDevelopmentMode(stderr_target)) {
            std.debug.print("\x1b[2J", .{}); // clear screen
            std.log.info("entered development mode '{s}'", .{stderr_target});
        } else |err| {
            std.log.warn("couldn't enable development mode: {s}", .{@errorName(err)});
        }
    }

    if (args.parse_file) |path| {
        const source = std.fs.cwd().readFileAlloc(allocator, path, 1 << 30) catch |err| {
            std.log.err("could not open '{s}': {s}", .{ path, @errorName(err) });
            return err;
        };
        defer allocator.free(source);

        var diagnostics = std.ArrayList(parse.Diagnostic).init(allocator);
        defer diagnostics.deinit();

        var tree = try parse.parse(allocator, source, .{ .diagnostics = &diagnostics });
        defer tree.deinit(allocator);

        if (args.print_ast) {
            var buffered_stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
            try buffered_stdout.writer().print("{}", .{tree.format(source)});
            try buffered_stdout.flush();
        }

        if (diagnostics.items.len != 0) {
            for (diagnostics.items) |diagnostic| {
                const position = diagnostic.position(source);
                try std.io.getStdErr().writer().print(
                    "{s}:{}:{}: {s}\n",
                    .{ path, position.line + 1, position.character + 1, diagnostic.message },
                );
            }
            return 1;
        }

        return 0;
    }

    var channel: Channel = switch (args.channel) {
        .stdio => .{ .stdio = .{
            .stdout = std.io.getStdOut(),
            .stdin = std.io.getStdIn(),
        } },
        .socket => |port| blk: {
            if (builtin.os.tag == .wasi) {
                return error.UnsupportedPlatform;
            }

            const address = try std.net.Address.parseIp("127.0.0.1", port);

            var server = try address.listen(.{});
            defer server.deinit();

            const connection = try server.accept();
            std.log.info("incoming connection from {}", .{connection.address});
            break :blk .{ .socket = connection.stream };
        },
    };
    defer channel.close();

    var buffered_writer = std.io.bufferedWriter(channel.writer());
    var state = State{
        .allocator = allocator,
        .channel = &buffered_writer,
        .workspace = try Workspace.init(allocator),
    };
    defer state.deinit();

    var buffered_reader = std.io.bufferedReader(channel.reader());
    const reader = buffered_reader.reader();

    var header_buffer: [1024]u8 = undefined;
    var header_stream = std.io.fixedBufferStream(&header_buffer);

    var content_buffer = std.ArrayList(u8).init(allocator);
    defer content_buffer.deinit();

    var parse_arena = std.heap.ArenaAllocator.init(allocator);
    defer parse_arena.deinit();

    const max_content_length = 4 << 20; // 4MB

    outer: while (state.running) {
        defer _ = parse_arena.reset(.retain_capacity);

        // read headers
        const headers = blk: {
            header_stream.reset();
            while (!std.mem.endsWith(u8, header_buffer[0..header_stream.pos], "\r\n\r\n")) {
                reader.streamUntilDelimiter(header_stream.writer(), '\n', null) catch |err| {
                    if (err == error.EndOfStream) break :outer;
                    return err;
                };
                _ = try header_stream.write("\n");
            }
            break :blk try parseHeaders(header_buffer[0..header_stream.pos]);
        };

        // read content
        const contents = blk: {
            if (headers.content_length > max_content_length) return error.MessageTooLong;
            try content_buffer.resize(headers.content_length);
            const actual_length = try reader.readAll(content_buffer.items);
            if (actual_length < headers.content_length) return error.UnexpectedEof;
            break :blk content_buffer.items;
        };

        // parse message(s)
        var message = blk: {
            var scanner = std.json.Scanner.initCompleteInput(parse_arena.allocator(), contents);
            defer scanner.deinit();

            var diagnostics = std.json.Diagnostics{};
            scanner.enableDiagnostics(&diagnostics);

            break :blk std.json.parseFromTokenSourceLeaky(
                rpc.Message(Request),
                parse_arena.allocator(),
                &scanner,
                .{ .allocate = .alloc_if_needed },
            ) catch |err| {
                logJsonError(@errorName(err), diagnostics, contents);
                state.fail(.null, .{ .code = .parse_error, .message = @errorName(err) }) catch {};
                continue;
            };
        };

        state.handleMessage(&message) catch |err| switch (err) {
            error.Failure => continue,
            else => return err,
        };
    }

    return 0;
}

fn logJsonError(err: []const u8, diagnostics: std.json.Diagnostics, bytes: []const u8) void {
    std.log.err("{}:{}: {s}: '{'}'", .{
        diagnostics.getLine(),
        diagnostics.getColumn(),
        err,
        std.zig.fmtEscapes(util.getJsonErrorContext(diagnostics, bytes)),
    });
}

const HeaderValues = struct {
    content_length: u32,
    mime_type: []const u8,
};

fn parseHeaders(bytes: []const u8) !HeaderValues {
    var content_length: ?u32 = null;
    var mime_type: []const u8 = "application/vscode-jsonrpc; charset=utf-8";

    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trimRight(u8, line, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;

        const colon = std.mem.indexOfScalar(u8, trimmed, ':') orelse return error.InvalidHeader;

        const name = trimmed[0..colon];
        const value = std.mem.trim(u8, trimmed[colon + 1 ..], &std.ascii.whitespace);

        if (std.ascii.eqlIgnoreCase(name, "Content-Length")) {
            content_length = try std.fmt.parseInt(u32, value, 10);
        } else if (std.ascii.eqlIgnoreCase(name, "Content-Type")) {
            mime_type = value;
        }
    }

    return HeaderValues{
        .content_length = content_length orelse return error.MissingContentLength,
        .mime_type = mime_type,
    };
}

pub const Channel = union(enum) {
    stdio: struct {
        stdin: std.fs.File,
        stdout: std.fs.File,
    },
    socket: std.net.Stream,

    pub fn close(self: *Channel) void {
        switch (self.*) {
            .stdio => {},
            .socket => |stream| stream.close(),
        }
    }

    pub const Reader = std.io.Reader(*Channel, ReadError, read);
    pub const ReadError = std.fs.File.ReadError || std.net.Stream.ReadError;

    pub fn read(self: *Channel, buffer: []u8) ReadError!usize {
        switch (self.*) {
            .stdio => |stdio| return stdio.stdin.read(buffer),
            .socket => |stream| return stream.read(buffer),
        }
    }

    pub fn reader(self: *Channel) Reader {
        return .{ .context = self };
    }

    pub const Writer = std.io.Writer(*Channel, WriteError, write);
    pub const WriteError = std.fs.File.WriteError || std.net.Stream.WriteError;

    pub fn write(self: *Channel, bytes: []const u8) WriteError!usize {
        switch (self.*) {
            .stdio => |stdio| return stdio.stdout.write(bytes),
            .socket => |stream| return stream.write(bytes),
        }
    }

    pub fn writer(self: *Channel) Writer {
        return .{ .context = self };
    }
};

const State = struct {
    allocator: std.mem.Allocator,

    channel: *std.io.BufferedWriter(4096, Channel.Writer),
    running: bool = true,
    initialized: bool = false,
    parent_pid: ?c_int = null,
    workspace: Workspace,

    pub fn deinit(self: *State) void {
        self.workspace.deinit();
    }

    fn handleMessage(self: *State, message: *rpc.Message(Request)) !void {
        switch (message.*) {
            .single => |*request| try self.dispatchRequest(request),
            .batch => |batch| try self.dispatchBatch(batch),
        }
    }

    fn dispatchBatch(self: *State, requests: []Request) !void {
        for (requests) |*request| try self.dispatchRequest(request);
    }

    fn dispatchRequest(self: *State, request: *Request) !void {
        if (!std.mem.eql(u8, request.jsonrpc, "2.0"))
            return self.fail(request.id, .{
                .code = .invalid_request,
                .message = "invalid jsonrpc version",
            });

        std.log.debug("method: '{'}'", .{std.zig.fmtEscapes(request.method)});

        if (!self.initialized and !std.mem.eql(u8, request.method, "initialize"))
            return self.fail(request.id, .{
                .code = .server_not_initialized,
                .message = "server has not been initialized",
            });

        inline for (Dispatch.methods) |method| {
            if (std.mem.eql(u8, request.method, method)) {
                try @field(Dispatch, method)(self, request);
                break;
            }
        } else {
            return self.fail(request.id, .{
                .code = .method_not_found,
                .message = request.method,
            });
        }
    }

    const SendError = Channel.WriteError;

    fn sendResponse(self: *State, response: *const Response) SendError!void {
        const format_options = std.json.StringifyOptions{
            .emit_null_optional_fields = false,
        };

        // get the size of the encoded message
        var counting = std.io.countingWriter(std.io.null_writer);
        try std.json.stringify(response, format_options, counting.writer());
        const content_length = counting.bytes_written;

        // send the message to the client
        const writer = self.channel.writer();
        try writer.print("Content-Length: {}\r\n\r\n", .{content_length});
        try std.json.stringify(response, format_options, writer);
        try self.channel.flush();
    }

    pub fn fail(
        self: *State,
        id: Request.Id,
        err: lsp.Error,
    ) (error{Failure} || SendError) {
        try self.sendResponse(&.{ .id = id, .result = .{ .failure = err } });
        return error.Failure;
    }

    pub fn success(self: *State, id: Request.Id, data: anytype) !void {
        const bytes = try std.json.stringifyAlloc(self.allocator, data, .{});
        defer self.allocator.free(bytes);
        try self.sendResponse(&Response{ .id = id, .result = .{ .success = .{ .raw = bytes } } });
    }
};

const LineStart = struct {
    /// The utf-8 byte offset.
    utf8: u32,
    /// The utf-16 byte offset.
    utf16: u32,
};

const Diagnostic = struct {
    message: ?[]const u8 = null,
    /// If the message has been allocated, this is `false`, otherwise `true`.
    static: bool = true,
};

pub const Dispatch = struct {
    pub const methods = [_][]const u8{
        "initialize",
        "initialized",
        "shutdown",
        "textDocument/didOpen",
        "textDocument/didClose",
        "textDocument/didSave",
        "textDocument/didChange",
        "textDocument/completion",
        "textDocument/hover",
        "textDocument/formatting",
        "textDocument/definition",
    };

    fn parseParams(comptime T: type, state: *State, request: *Request) !std.json.Parsed(T) {
        return std.json.parseFromValue(T, state.allocator, request.params, .{
            .ignore_unknown_fields = true,
        });
    }

    fn getDocumentOrFail(
        state: *State,
        request: *Request,
        id: lsp.TextDocumentIdentifier,
    ) !*Workspace.Document {
        return try state.workspace.getDocument(id) orelse state.fail(request.id, .{
            .code = .invalid_params,
            .message = "document not found",
        });
    }

    pub const InitializeParams = struct {
        processId: ?c_int = null,
        clientInfo: ?struct {
            name: []const u8,
            version: ?[]const u8 = null,
        } = null,
        capabilities: lsp.ClientCapabilities,
    };

    pub fn initialize(state: *State, request: *Request) !void {
        if (state.initialized) {
            return state.fail(request.id, .{
                .code = .invalid_request,
                .message = "server already initialized",
            });
        }

        const params = try parseParams(InitializeParams, state, request);
        defer params.deinit();

        try state.success(request.id, .{
            .capabilities = .{
                .completionProvider = .{
                    .triggerCharacters = .{"."},
                },
                .textDocumentSync = .{
                    .openClose = true,
                    .change = @intFromEnum(lsp.TextDocumentSyncKind.incremental),
                    .willSave = false,
                    .willSaveWaitUntil = false,
                    .save = .{ .includeText = false },
                },
                .hoverProvider = true,
                .documentFormattingProvider = true,
                .definitionProvider = true,
            },
            .serverInfo = .{ .name = "glsl_analyzer" },
        });

        state.initialized = true;
        state.parent_pid = state.parent_pid orelse params.value.processId;
    }

    pub fn shutdown(state: *State, request: *Request) !void {
        state.running = false;
        try state.success(request.id, null);
    }

    pub fn initialized(state: *State, request: *Request) !void {
        _ = request;
        _ = state;
        return;
    }

    pub const DidOpenParams = struct {
        textDocument: lsp.TextDocumentItem,
    };

    pub fn @"textDocument/didOpen"(state: *State, request: *Request) !void {
        const params = try parseParams(DidOpenParams, state, request);
        defer params.deinit();

        const document = &params.value.textDocument;
        std.log.debug("opened: {s} : {s} : {} : {}", .{
            document.uri,
            document.languageId,
            document.version,
            document.text.len,
        });

        const file = try state.workspace.getOrCreateDocument(document.versioned());
        try file.replaceAll(document.text);

        return;
    }

    pub const DidCloseParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
    };

    pub fn @"textDocument/didClose"(state: *State, request: *Request) !void {
        const params = try parseParams(DidCloseParams, state, request);
        defer params.deinit();
        std.log.debug("closed: {s}", .{params.value.textDocument.uri});
        return;
    }

    pub const DidSaveParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
        text: ?[]const u8 = null,
    };

    pub fn @"textDocument/didSave"(state: *State, request: *Request) !void {
        const params = try parseParams(DidSaveParams, state, request);
        defer params.deinit();

        std.log.debug("saved: {s} : {?}", .{
            params.value.textDocument.uri,
            if (params.value.text) |text| text.len else null,
        });

        return;
    }

    pub const DidChangeParams = struct {
        textDocument: lsp.VersionedTextDocumentIdentifier,
        contentChanges: []const lsp.TextDocumentContentChangeEvent,
    };

    pub fn @"textDocument/didChange"(state: *State, request: *Request) !void {
        const params = try parseParams(DidChangeParams, state, request);
        defer params.deinit();

        std.log.debug("didChange: {s}", .{params.value.textDocument.uri});
        const file = try state.workspace.getOrCreateDocument(params.value.textDocument);

        for (params.value.contentChanges) |change| {
            if (change.range) |range| {
                try file.replace(range, change.text);
            } else {
                try file.replaceAll(change.text);
            }
        }

        file.version = params.value.textDocument.version;
    }

    pub const CompletionParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
        position: lsp.Position,
    };

    pub fn @"textDocument/completion"(state: *State, request: *Request) !void {
        const params = try parseParams(CompletionParams, state, request);
        defer params.deinit();

        std.log.debug("complete: {} {s}", .{ params.value.position, params.value.textDocument.uri });

        const document = try getDocumentOrFail(state, request, params.value.textDocument);

        var completions = std.ArrayList(lsp.CompletionItem).init(state.allocator);
        defer completions.deinit();

        var symbol_arena = std.heap.ArenaAllocator.init(state.allocator);
        defer symbol_arena.deinit();

        const token = try document.tokenBeforeCursor(params.value.position);

        const parsed = try document.parseTree();
        if (token != null and parsed.tree.tag(token.?) == .comment) {
            // don't give completions in comments
            return state.success(request.id, null);
        }

        try completionsAtToken(
            state,
            document,
            token,
            &completions,
            symbol_arena.allocator(),
            .{ .ignore_current = true },
        );

        try state.success(request.id, completions.items);
    }

    fn completionsAtToken(
        state: *State,
        document: *Workspace.Document,
        start_token: ?u32,
        completions: *std.ArrayList(lsp.CompletionItem),
        arena: std.mem.Allocator,
        options: struct { ignore_current: bool },
    ) !void {
        var has_fields = false;

        var symbols = std.ArrayList(analysis.Reference).init(arena);

        if (start_token) |token| {
            try analysis.visibleFields(arena, document, token, &symbols);
            has_fields = symbols.items.len != 0;

            if (!has_fields) try analysis.visibleSymbols(arena, document, token, &symbols);

            try completions.ensureUnusedCapacity(symbols.items.len);

            for (symbols.items) |symbol| {
                if (options.ignore_current and symbol.document == document and symbol.node == token) {
                    continue;
                }

                const parsed: *const Workspace.Document.CompleteParseTree = try symbol.document.parseTree();

                const symbol_type = try analysis.typeOf(symbol);

                const type_signature = if (symbol_type) |typ|
                    try std.fmt.allocPrint(arena, "{}", .{
                        typ.format(parsed.tree, symbol.document.source()),
                    })
                else if (parsed.tree.tag(symbol.node) == .preprocessor) blk: {
                    const span = symbol.span();
                    break :blk symbol.document.source()[span.start..span.end];
                } else null;

                try completions.append(.{
                    .label = symbol.name(),
                    .labelDetails = .{
                        .detail = type_signature,
                    },
                    .detail = type_signature,
                    .kind = switch (parsed.tree.tag(symbol.parent_declaration)) {
                        .struct_specifier => .class,
                        .function_declaration => .function,
                        .preprocessor => .constant,
                        else => blk: {
                            if (parsed.tree.parent(symbol.parent_declaration)) |grandparent| {
                                if (parsed.tree.tag(grandparent) == .field_declaration_list) {
                                    break :blk .field;
                                }
                            }
                            break :blk .variable;
                        },
                    },
                });
            }
        }

        if (!has_fields) {
            try completions.appendSlice(state.workspace.builtin_completions);
        }
    }

    pub const HoverParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
        position: lsp.Position,
    };

    pub fn @"textDocument/hover"(state: *State, request: *Request) !void {
        const params = try parseParams(HoverParams, state, request);
        defer params.deinit();

        std.log.debug("hover: {} {s}", .{ params.value.position, params.value.textDocument.uri });

        const document = try getDocumentOrFail(state, request, params.value.textDocument);
        const parsed = try document.parseTree();

        const token = try document.identifierUnderCursor(params.value.position) orelse {
            return state.success(request.id, null);
        };

        if (parsed.tree.tag(token) != .identifier) {
            return state.success(request.id, null);
        }

        const token_span = parsed.tree.token(token);
        const token_text = document.source()[token_span.start..token_span.end];

        var completions = std.ArrayList(lsp.CompletionItem).init(state.allocator);
        defer completions.deinit();

        var symbol_arena = std.heap.ArenaAllocator.init(state.allocator);
        defer symbol_arena.deinit();

        try completionsAtToken(
            state,
            document,
            token,
            &completions,
            symbol_arena.allocator(),
            .{ .ignore_current = false },
        );

        // group completions by their documentation.
        var groups = std.StringArrayHashMap(std.ArrayListUnmanaged(*const lsp.CompletionItem))
            .init(symbol_arena.allocator());
        defer groups.deinit();

        for (completions.items) |*completion| {
            if (!std.mem.eql(u8, completion.label, token_text)) continue;

            const documentation_string = if (completion.documentation) |markup| markup.value else "";

            const result = try groups.getOrPut(documentation_string);
            if (!result.found_existing) result.value_ptr.* = .{};
            try result.value_ptr.append(symbol_arena.allocator(), completion);
        }

        var text = std.ArrayList(u8).init(symbol_arena.allocator());
        defer text.deinit();

        for (groups.keys(), groups.values()) |description, group| {
            if (text.items.len != 0) {
                try text.appendSlice("\n\n---\n\n");
            }

            if (group.items.len != 0) {
                try text.appendSlice("```glsl\n");
                for (group.items) |completion| {
                    if (completion.detail) |detail| {
                        try text.writer().print("{s}\n", .{detail});
                    }
                }
                try text.appendSlice("```\n");
            }

            if (description.len != 0) {
                if (group.items.len != 0) try text.appendSlice("\n");
                try text.appendSlice(description);
            }
        }

        if (text.items.len == 0) {
            return state.success(request.id, null);
        }

        try state.success(request.id, .{
            .contents = lsp.MarkupContent{
                .kind = .markdown,
                .value = text.items,
            },
        });
    }

    const FormattingParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
        options: struct {
            tabSize: u32 = 4,
            insertSpaces: bool = true,
        },
    };

    pub fn @"textDocument/formatting"(state: *State, request: *Request) !void {
        const params = try parseParams(FormattingParams, state, request);
        defer params.deinit();
        std.log.debug("format: {s}", .{params.value.textDocument.uri});

        const document = try state.workspace.getOrLoadDocument(params.value.textDocument);
        const parsed = try document.parseTree();

        var buffer = std.ArrayList(u8).init(state.allocator);
        defer buffer.deinit();

        try @import("format.zig").format(
            parsed.tree,
            document.contents.items,
            buffer.writer(),
            .{ .ignored = parsed.ignored },
        );

        try state.success(request.id, .{
            .{
                .range = document.wholeRange(),
                .newText = buffer.items,
            },
        });
    }

    const DefinitionParams = struct {
        textDocument: lsp.TextDocumentIdentifier,
        position: lsp.Position,
    };

    pub fn @"textDocument/definition"(state: *State, request: *Request) !void {
        const params = try parseParams(DefinitionParams, state, request);
        defer params.deinit();
        std.log.debug("goto definition: {} {s}", .{
            params.value.position,
            params.value.textDocument.uri,
        });

        const document = try state.workspace.getOrLoadDocument(params.value.textDocument);
        const source_node = try document.identifierUnderCursor(params.value.position) orelse {
            std.log.debug("no node under cursor", .{});
            return state.success(request.id, null);
        };

        var references = std.ArrayList(analysis.Reference).init(state.allocator);
        defer references.deinit();

        var arena = std.heap.ArenaAllocator.init(state.allocator);
        defer arena.deinit();
        try analysis.findDefinition(arena.allocator(), document, source_node, &references);

        if (references.items.len == 0) {
            std.log.debug("could not find definition", .{});
            return state.success(request.id, null);
        }

        const locations = try state.allocator.alloc(lsp.Location, references.items.len);
        defer state.allocator.free(locations);

        for (references.items, locations) |reference, *location| {
            location.* = .{
                .uri = reference.document.uri,
                .range = try reference.document.nodeRange(reference.node),
            };
        }

        try state.success(request.id, locations);
    }
};

test {
    std.testing.refAllDeclsRecursive(@This());
    std.testing.refAllDeclsRecursive(@import("Workspace.zig"));
    std.testing.refAllDeclsRecursive(@import("Document.zig"));
    std.testing.refAllDeclsRecursive(@import("parse.zig"));
    std.testing.refAllDeclsRecursive(@import("format.zig"));
    std.testing.refAllDeclsRecursive(@import("analysis.zig"));
    std.testing.refAllDeclsRecursive(@import("syntax.zig"));
}
