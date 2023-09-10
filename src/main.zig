const std = @import("std");
const build_options = @import("build_options");

const util = @import("util.zig");
const lsp = @import("lsp.zig");
const rpc = @import("jsonrpc.zig");
const Response = rpc.Response;
const Request = rpc.Request;

const Spec = @import("Spec.zig");
const Workspace = @import("Workspace.zig");
const cli = @import("cli.zig");

pub const std_options = struct {
    pub const log_level = .info;
};

const stderr_target = build_options.build_root ++ "/stderr.log";

fn enableDevelopmentMode() !void {
    // redirect stderr to the build root
    const O = std.os.O;
    const S = std.os.S;
    const new_stderr = try std.os.open(
        stderr_target,
        O.WRONLY | O.CREAT | O.TRUNC,
        S.IRUSR | S.IWUSR | S.IRGRP | S.IROTH,
    );
    try std.os.dup2(new_stderr, std.os.STDERR_FILENO);
    std.os.close(new_stderr);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var static_arena = std.heap.ArenaAllocator.init(allocator);
    defer static_arena.deinit();

    const args = try cli.Arguments.parse(allocator);

    if (args.dev_mode) {
        try enableDevelopmentMode();
        std.debug.print("\x1b[2J", .{}); // clear screen
        std.log.info("entered development mode '{s}'", .{stderr_target});
    }

    var channel: Channel = switch (args.channel) {
        .stdio => .{ .stdio = .{
            .stdout = std.io.getStdOut(),
            .stdin = std.io.getStdIn(),
        } },
        .socket => |port| blk: {
            var server = std.net.StreamServer.init(.{});
            defer server.deinit();

            try server.listen(try std.net.Address.parseIp("127.0.0.1", port));
            const connection = try server.accept();
            std.log.info("incoming connection from {}", .{connection.address});
            break :blk .{ .socket = connection.stream };
        },
    };
    defer channel.close();

    const spec = try Spec.load(allocator);
    defer spec.deinit();

    const builtin_completions = try builtinCompletions(static_arena.allocator(), &spec.value);

    var buffered_writer = std.io.bufferedWriter(channel.writer());
    var state = State{
        .allocator = allocator,
        .channel = &buffered_writer,
        .spec = &spec.value,
        .builtin_completions = builtin_completions,
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

    outer: while (true) {
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

        defer _ = parse_arena.reset(.retain_capacity);

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
    initialized: bool = false,
    parent_pid: ?c_int = null,
    spec: *const Spec,
    workspace: Workspace = .{},
    builtin_completions: []const lsp.CompletionItem,

    pub fn deinit(self: *State) void {
        self.workspace.deinit(self.allocator);
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
        return state.workspace.getDocument(id) orelse state.fail(request.id, .{
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
                .completionProvider = .{},
                .textDocumentSync = .{
                    .openClose = true,
                    .change = @intFromEnum(lsp.TextDocumentSyncKind.incremental),
                    .willSave = false,
                    .willSaveWaitUntil = false,
                    .save = .{ .includeText = false },
                },
                .hoverProvider = true,
            },
            .serverInfo = .{ .name = "glsl_analyzer" },
        });

        state.initialized = true;
        state.parent_pid = state.parent_pid orelse params.value.processId;
    }

    pub fn shutdown(state: *State, request: *Request) !void {
        _ = request;
        _ = state;
        return;
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

        const file = try state.workspace.getOrCreateDocument(state.allocator, document.versioned());
        try file.replaceAll(state.allocator, document.text);

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
        const file: *Workspace.Document = try state.workspace.getOrCreateDocument(
            state.allocator,
            params.value.textDocument,
        );

        for (params.value.contentChanges) |change| {
            if (change.range) |range| {
                try file.replace(state.allocator, range, change.text);
            } else {
                try file.replaceAll(state.allocator, change.text);
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

        var completions = std.ArrayList(lsp.CompletionItem).init(state.allocator);
        defer completions.deinit();

        try completions.appendSlice(state.builtin_completions);

        try state.success(request.id, completions.items);
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
        const word = document.wordUnderCursor(params.value.position);

        std.log.debug("hover word: '{'}'", .{std.zig.fmtEscapes(word)});

        const contents = for (state.builtin_completions) |*completion| {
            if (std.mem.eql(u8, completion.label, word)) {
                if (completion.documentation) |*doc| break doc;
                std.log.debug("item without documentation: '{'}'", .{std.zig.fmtEscapes(completion.label)});
            }
        } else {
            return state.success(request.id, null);
        };

        try state.success(request.id, .{
            .contents = contents,
        });
    }
};

fn builtinCompletions(arena: std.mem.Allocator, spec: *const Spec) ![]lsp.CompletionItem {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena);

    const types = [_][]const u8{
        "void",
        "bool",
        "int",
        "uint",
        "float",
        "double",
        "vec2",
        "vec3",
        "vec4",
        "ivec2",
        "ivec3",
        "ivec4",
        "uvec2",
        "uvec3",
        "uvec4",
        "bvec2",
        "bvec3",
        "bvec4",
        "dvec2",
        "dvec3",
        "dvec4",
        "mat2",
        "mat3",
        "mat4",
    };

    try completions.ensureUnusedCapacity(types.len + spec.variables.len + spec.functions.len);

    for (types) |name| {
        try completions.append(.{ .label = name, .kind = .class });
    }

    for (spec.variables) |variable| {
        var signature = std.ArrayList(u8).init(arena);
        try signature.writer().print("{}", .{variable.modifiers});
        try signature.appendSlice(" ");
        try signature.appendSlice(variable.type);

        try completions.append(.{
            .label = variable.name,
            .labelDetails = .{
                .detail = try signature.toOwnedSlice(),
            },
            .kind = .variable,
            .documentation = if (variable.description) |desc| .{
                .kind = .markdown,
                .value = try std.mem.join(arena, "\n\n", desc),
            } else null,
        });
    }

    for (spec.functions) |function| {
        var anonymous_signature = std.ArrayList(u8).init(arena);
        var named_signature = std.ArrayList(u8).init(arena);
        try writeFunctionSignature(function, anonymous_signature.writer(), .{ .names = false });
        try writeFunctionSignature(function, named_signature.writer(), .{ .names = true });

        try completions.append(.{
            .label = function.name,
            .labelDetails = .{
                .detail = try anonymous_signature.toOwnedSlice(),
            },
            .kind = .function,
            .detail = try named_signature.toOwnedSlice(),
            .documentation = if (function.description) |desc| .{
                .kind = .markdown,
                .value = try std.mem.join(arena, "\n\n", desc),
            } else null,
        });
    }

    return completions.toOwnedSlice();
}

fn writeFunctionSignature(function: Spec.Function, writer: anytype, options: struct { names: bool }) !void {
    try writer.writeAll(function.return_type);
    try writer.writeAll(" ");
    if (options.names) try writer.writeAll(function.name);
    try writer.writeAll("(");
    for (function.parameters, 0..) |param, i| {
        if (i != 0) try writer.writeAll(", ");
        if (param.optional) try writer.writeAll("[");
        if (param.modifiers) |modifiers| {
            try writer.print("{}", .{modifiers});
            try writer.writeAll(" ");
        }
        if (options.names) {
            const array_start = std.mem.indexOfScalar(u8, param.type, '[') orelse param.type.len;
            try writer.writeAll(param.type[0..array_start]);
            try writer.writeAll(" ");
            try writer.writeAll(param.name);
            try writer.writeAll(param.type[array_start..]);
        } else {
            try writer.writeAll(param.type);
        }
        if (param.optional) try writer.writeAll("]");
    }
    try writer.writeAll(")");
}
