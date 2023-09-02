const std = @import("std");

const NAME = "glsl_analyzer";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try Arguments.parse(allocator);

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

    var buffered_reader = std.io.bufferedReader(channel.reader());
    const reader = buffered_reader.reader();

    var header_buffer: [1024]u8 = undefined;

    outer: while (true) {
        var header_stream = std.io.fixedBufferStream(&header_buffer);

        while (!std.mem.endsWith(u8, header_buffer[0..header_stream.pos], "\r\n\r\n")) {
            reader.streamUntilDelimiter(header_stream.writer(), '\n', null) catch |err| {
                if (err == error.EndOfStream) break :outer;
                return err;
            };
            _ = try header_stream.write("\n");
        }

        const headers = try parseHeaders(header_buffer[0..header_stream.pos]);

        const buffer = try allocator.alloc(u8, headers.content_length);
        defer allocator.free(buffer);

        const actual_length = try reader.readAll(buffer);
        if (actual_length != buffer.len) return error.UnexpectedEof;

        const request = try std.json.parseFromSlice(Request, allocator, buffer, .{
            .allocate = .alloc_if_needed,
        });
        defer request.deinit();

        try std.json.stringify(
            request.value,
            .{ .whitespace = .indent_2 },
            std.io.getStdErr().writer(),
        );
    }
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

        const name = std.mem.trimRight(u8, trimmed[0..colon], &std.ascii.whitespace);
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

pub const Arguments = struct {
    channel: ChannelKind = .stdio,
    client_pid: ?c_int = null,

    pub const ChannelKind = union(enum) {
        stdio: void,
        socket: u16,
    };

    const usage =
        "Usage: " ++ NAME ++
        \\ [OPTIONS]
        \\
        \\Options:
        \\      --stdio                   Communicate over stdio [default]
        \\  -p, --port <PORT>             Communicate over socket
        \\      --clientProcessId <PID>   PID of the client process
        \\
        \\
    ;

    fn printUsage() noreturn {
        std.io.getStdErr().writer().writeAll(usage) catch {};
        std.process.exit(1);
    }

    fn fail(comptime fmt: []const u8, args: anytype) noreturn {
        std.io.getStdErr().writer().writeAll(usage) catch {};
        std.log.err(fmt ++ "\n", args);
        std.process.exit(1);
    }

    fn parse(allocator: std.mem.Allocator) !Arguments {
        var args = try std.process.argsWithAllocator(allocator);
        defer args.deinit();
        _ = args.skip();

        var parsed = Arguments{};

        while (args.next()) |arg| {
            const name_end = std.mem.indexOfScalar(u8, arg, '=') orelse arg.len;
            const name = arg[0..name_end];
            const extra_value = if (name_end == arg.len) null else arg[name_end + 1 ..];

            if (isAny(name, &.{ "--help", "-h" })) {
                printUsage();
            }

            if (isAny(name, &.{"--stdio"})) {
                parsed.channel = .stdio;
                continue;
            }

            if (isAny(name, &.{ "--port", "-p" })) {
                const value = extra_value orelse args.next() orelse
                    fail("expected port number", .{});
                const port = std.fmt.parseInt(u16, value, 10) catch
                    fail("not a valid port number: {s}", .{value});
                parsed.channel = .{ .socket = port };
                continue;
            }

            if (isAny(name, &.{"--clientProcessId"})) {
                const value = extra_value orelse args.next() orelse fail("expected PID", .{});
                parsed.client_pid = std.fmt.parseInt(c_int, value, 10) catch
                    fail("not a valid PID: {s}", .{value});
                continue;
            }

            fail("unexpected argument '{s}'", .{arg});
        }

        return parsed;
    }

    fn isAny(name: []const u8, expected: []const []const u8) bool {
        for (expected) |string| {
            if (std.mem.eql(u8, name, string)) return true;
        } else {
            return false;
        }
    }
};

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

const Message = union(enum) {
    list: []Request,
    request: Request,
    respnse: Response,
};

const Request = struct {
    pub const Id = union(enum) {
        integer: i64,
        string: []const u8,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            switch (self) {
                .integer => |int| try jw.write(int),
                .string => |text| try jw.write(text),
            }
        }

        pub fn jsonParse(
            allocator: std.mem.Allocator,
            source: anytype,
            options: std.json.ParseOptions,
        ) !@This() {
            _ = options;
            var id: Id = undefined;
            switch (try source.nextAlloc(allocator, .alloc_if_needed)) {
                .number => |int| id = .{ .integer = try std.fmt.parseInt(i64, int, 10) },
                .allocated_number, .allocated_string => |text| id = .{ .string = text },
                .string => |text| id = .{ .string = try allocator.dupe(u8, text) },
                else => return error.UnexpectedToken,
            }
            return id;
        }
    };

    jsonrpc: []const u8 = "2.0",
    method: []const u8,
    id: ?Id,
    params: std.json.Value,
};

const Response = struct {
    jsonrpc: []const u8 = "2.0",
    id: Request.Id,
    result: std.json.Value = .null,
    @"error": ?Error = null,

    pub const Error = struct {
        code: Code,
        message: []const u8,
        data: std.json.Value = .null,

        pub const Code = enum(i32) {
            parse_error = -32700,
            invalid_request = -32600,
            method_not_found = -32601,
            invalid_params = -32502,
            internal_error = -32603,

            // jsonrpc reserved:
            server_not_initialized = -32002,
            unknown_error_code = -32001,

            // LSP reserved:
            request_failed = -32803,
            server_cancelled = -32802,
            content_modified = -32801,
            request_cancelled = -32800,

            _,

            pub fn jsonStringify(self: *@This(), jw: anytype) !void {
                try jw.write(@intFromEnum(self.*));
            }
        };
    };
};
