const std = @import("std");

pub const NAME = "glsl_analyzer";

pub const Arguments = struct {
    channel: ChannelKind = .stdio,
    client_pid: ?c_int = null,
    dev_mode: bool = false,

    pub const ChannelKind = union(enum) {
        stdio: void,
        socket: u16,
    };

    const usage =
        "Usage: " ++ NAME ++
        \\ [OPTIONS]
        \\
        \\Options:
        \\     --stdio                  Communicate over stdio [default]
        \\ -p, --port <PORT>            Communicate over socket
        \\     --clientProcessId <PID>  PID of the client process
        \\     --dev-mode               Enable development mode
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

    pub fn parse(allocator: std.mem.Allocator) !Arguments {
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

            if (isAny(name, &.{"--dev-mode"})) {
                parsed.dev_mode = true;
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
