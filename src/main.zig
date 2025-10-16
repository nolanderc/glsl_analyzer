const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const File = std.fs.File;

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

var stdout_buffer: [1024]u8 = undefined;

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

    if (args.format_file) |path| {
        const source = std.fs.cwd().readFileAlloc(allocator, path, 1 << 30) catch |err| {
            std.log.err("could not open '{s}': {s}", .{ path, @errorName(err) });
            return err;
        };
        defer allocator.free(source);

        var ignored = std.array_list.Managed(parse.Token).init(allocator);
        defer ignored.deinit();

        var diagnostics = std.array_list.Managed(parse.Diagnostic).init(allocator);
        defer diagnostics.deinit();

        var tree = try parse.parse(allocator, source, .{
            .ignored = &ignored,
            .diagnostics = &diagnostics,
        });
        defer tree.deinit(allocator);

        if (diagnostics.items.len != 0) {
            for (diagnostics.items) |diagnostic| {
                const position = diagnostic.position(source);
                var stderr_writer = std.fs.File.stderr().writer(&stdout_buffer);
                const stderr = &stderr_writer.interface;
                try stderr.print(
                    "{s}:{}:{}: {s}\n",
                    .{ path, position.line + 1, position.character + 1, diagnostic.message },
                );
            }
            return 1;
        }

        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const stdout = &stdout_writer.interface;

        try @import("format.zig").format(tree, source, stdout, .{
            .ignored = ignored.items,
        });
        try stdout.flush();

        return 0;
    }

    if (args.parse_file) |path| {
        const source = std.fs.cwd().readFileAlloc(allocator, path, 1 << 30) catch |err| {
            std.log.err("could not open '{s}': {s}", .{ path, @errorName(err) });
            return err;
        };
        defer allocator.free(source);

        var diagnostics = std.array_list.Managed(parse.Diagnostic).init(allocator);
        defer diagnostics.deinit();

        var tree = try parse.parse(allocator, source, .{ .diagnostics = &diagnostics });
        defer tree.deinit(allocator);

        if (args.print_ast) {
            var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
            const stdout = &stdout_writer.interface;

            try stdout.print("{f}", .{tree.format(source)});
            try stdout.flush();
        }

        if (diagnostics.items.len != 0) {
            for (diagnostics.items) |diagnostic| {
                const position = diagnostic.position(source);
                var stderr_writer = std.fs.File.stderr().writer(&stdout_buffer);
                const stderr = &stderr_writer.interface;
                try stderr.print(
                    "{s}:{}:{}: {s}\n",
                    .{ path, position.line + 1, position.character + 1, diagnostic.message },
                );
                try stderr.flush();
            }
            return 1;
        }

        return 0;
    }

    var channel: Channel = switch (args.channel) {
        .stdio => .{ .stdio = .{
            .stdout = std.fs.File.stdout(),
            .stdin = std.fs.File.stdin(),
        } },
        .socket => |port| blk: {
            if (builtin.os.tag == .wasi) {
                return error.UnsupportedPlatform;
            }

            const address = try std.net.Address.parseIp("127.0.0.1", port);

            var server = try address.listen(.{});
            defer server.deinit();

            const connection = try server.accept();
            std.log.info("incoming connection from {f}", .{connection.address});
            break :blk .{ .socket = connection.stream };
        },
    };
    defer channel.close();

    var channel_buffer: [1024]u8 = undefined;
    var channel_writer = channel.writer(&channel_buffer);
    var state = State{
        .allocator = allocator,
        .channel = channel_writer.interface(),
        .workspace = try Workspace.init(allocator),
    };
    defer state.deinit();

    var reader_buffer: [1024]u8 = undefined;
    var reader = channel.reader(&reader_buffer);

    var header_buffer: [1024]u8 = undefined;
    var header_stream = std.Io.Writer.fixed(&header_buffer);

    var content_buffer = std.array_list.Managed(u8).init(allocator);
    defer content_buffer.deinit();

    var parse_arena = std.heap.ArenaAllocator.init(allocator);
    defer parse_arena.deinit();

    const max_content_length = 4 << 20; // 4MB

    outer: while (state.running) {
        defer _ = parse_arena.reset(.retain_capacity);

        // read headers
        const headers = blk: {
            header_stream.end = 0;
            while (!std.mem.endsWith(u8, header_buffer[0..header_stream.end], "\r\n\r\n")) {
                _ = reader.interface().streamDelimiter(&header_stream, '\n') catch |err| {
                    if (err == error.EndOfStream) break :outer;
                    return err;
                };

                reader.interface().toss(1);
                _ = try header_stream.write("\n");
            }
            break :blk try parseHeaders(header_buffer[0..header_stream.end]);
        };

        // read content
        const contents = blk: {
            if (headers.content_length > max_content_length) return error.MessageTooLong;
            try content_buffer.resize(headers.content_length);
            const actual_length = try reader.interface().readSliceShort(content_buffer.items);
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
    std.log.err("{}:{}: {s}: '{f}'", .{
        diagnostics.getLine(),
        diagnostics.getColumn(),
        err,
        std.zig.fmtString(util.getJsonErrorContext(diagnostics, bytes)),
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

    pub const ReadError = std.fs.File.ReadError || std.net.Stream.ReadError || std.io.Reader.Error;
    pub const Reader = struct {
        channel: *Channel,

        stdin_reader: std.fs.File.Reader = undefined,
        socket_reader: std.net.Stream.Reader = undefined,

        pub fn init(channel: *Channel, buffer: []u8) Reader {
            var r: Reader = .{
                .channel = channel,
            };

            switch (r.channel.*) {
                .stdio => |stdio| r.stdin_reader = stdio.stdin.reader(buffer),
                .socket => |stream| r.socket_reader = stream.reader(buffer),
            }

            return r;
        }

        pub fn interface(r: *Reader) *std.io.Reader {
            return switch (r.channel.*) {
                .stdio => &r.stdin_reader.interface,
                .socket => r.socket_reader.interface(),
            };
        }
    };

    pub fn reader(self: *Channel, buffer: []u8) Reader {
        return .init(self, buffer);
    }

    pub const WriteError = std.fs.File.WriteError || std.net.Stream.WriteError || std.io.Writer.Error;
    pub const Writer = struct {
        channel: *Channel,

        stdout_writer: std.fs.File.Writer = undefined,
        socket_writer: std.net.Stream.Writer = undefined,

        pub fn init(channel: *Channel, buffer: []u8) Writer {
            var w: Writer = .{
                .channel = channel,
            };

            switch (w.channel.*) {
                .stdio => |stdio| w.stdout_writer = stdio.stdout.writer(buffer),
                .socket => |stream| w.socket_writer = stream.writer(buffer),
            }

            return w;
        }

        pub fn interface(w: *Writer) *std.Io.Writer {
            return switch (w.channel.*) {
                .stdio => &w.stdout_writer.interface,
                .socket => &w.socket_writer.interface,
            };
        }
    };

    pub fn writer(self: *Channel, buffer: []u8) Writer {
        return .init(self, buffer);
    }
};

const State = struct {
    allocator: std.mem.Allocator,

    channel: *std.io.Writer,
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

        std.log.debug("method: '{f}'", .{std.zig.fmtString(request.method)});

        if (!self.initialized and !std.mem.eql(u8, request.method, "initialize"))
            return self.fail(request.id, .{
                .code = .server_not_initialized,
                .message = "server has not been initialized",
            });

        inline for (Dispatch.methods) |method| {
            if (std.mem.eql(u8, request.method, method)) {
                try @field(Dispatch, method)(self, request);
                return;
            }
        }

        // ignore unknown notifications
        if (request.id == .null) {
            std.log.debug("ignoring unknown '{f}' notification", .{std.zig.fmtString(request.method)});
            return;
        }

        return self.fail(request.id, .{
            .code = .method_not_found,
            .message = request.method,
        });
    }

    const SendError = Channel.WriteError;

    fn sendResponse(self: *State, response: *const Response) SendError!void {
        const format_options = std.json.Stringify.Options{
            .emit_null_optional_fields = false,
        };

        // get the size of the encoded message
        var counting: std.Io.Writer.Discarding = .init(&.{});
        try std.json.Stringify.value(response, format_options, &counting.writer);
        const content_length = counting.count;

        // send the message to the client
        try self.channel.print("Content-Length: {}\r\n\r\n", .{content_length});
        try std.json.Stringify.value(response, format_options, self.channel);
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
        const bytes = try std.json.Stringify.valueAlloc(self.allocator, data, .{});
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

        std.log.debug("complete: {f} {s}", .{ params.value.position, params.value.textDocument.uri });

        const document = try getDocumentOrFail(state, request, params.value.textDocument);

        var completions = std.array_list.Managed(lsp.CompletionItem).init(state.allocator);
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
        completions: *std.array_list.Managed(lsp.CompletionItem),
        arena: std.mem.Allocator,
        options: struct { ignore_current: bool },
    ) !void {
        var has_fields = false;

        var symbols = std.array_list.Managed(analysis.Reference).init(arena);

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
                    try std.fmt.allocPrint(arena, "{f}", .{
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

        std.log.debug("hover: {f} {s}", .{ params.value.position, params.value.textDocument.uri });

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

        var completions = std.array_list.Managed(lsp.CompletionItem).init(state.allocator);
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

        var text = std.array_list.Managed(u8).init(symbol_arena.allocator());
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
        std.log.debug("format: {s} tabSize: {}", .{ params.value.textDocument.uri, params.value.options.tabSize });

        const document = try state.workspace.getOrLoadDocument(params.value.textDocument);
        const parsed = try document.parseTree();

        var buffer: std.Io.Writer.Allocating = .init(state.allocator);
        defer buffer.deinit();

        try @import("format.zig").format(
            parsed.tree,
            document.contents.items,
            &buffer.writer,
            .{
                .ignored = parsed.ignored,
                .tab_size = params.value.options.tabSize,
            },
        );

        try state.success(request.id, .{
            .{
                .range = document.wholeRange(),
                .newText = buffer.written(),
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
        std.log.debug("goto definition: {f} {s}", .{
            params.value.position,
            params.value.textDocument.uri,
        });

        const document = try state.workspace.getOrLoadDocument(params.value.textDocument);
        const source_node = try document.identifierUnderCursor(params.value.position) orelse {
            std.log.debug("no node under cursor", .{});
            return state.success(request.id, null);
        };

        var references = std.array_list.Managed(analysis.Reference).init(state.allocator);
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
