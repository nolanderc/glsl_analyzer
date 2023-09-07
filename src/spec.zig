const std = @import("std");
const util = @import("util.zig");

pub const Spec = struct {
    comment: []const u8 = "",
    variables: []const Variable,
    functions: []const Function,

    pub const Variable = struct {
        modifier: Modifier = .in,
        type: []const u8,
        name: []const u8,
        description: []const []const u8,
        versions: []const u16,
    };

    pub const Function = struct {
        return_type: []const u8,
        name: []const u8,
        parameters: []const Parameter,
        description: []const []const u8,
        versions: []const u16,

        pub const Parameter = struct {
            modifier: ?Modifier = null,
            type: []const u8,
            name: []const u8,
            optional: bool = false,
        };
    };

    const bytes = @embedFile("glsl_spec.json");

    pub fn load(allocator: std.mem.Allocator) !std.json.Parsed(Spec) {
        var diagnostic = std.json.Diagnostics{};
        var scanner = std.json.Scanner.initCompleteInput(allocator, bytes);
        defer scanner.deinit();
        scanner.enableDiagnostics(&diagnostic);

        return std.json.parseFromTokenSource(Spec, allocator, &scanner, .{}) catch |err| {
            std.log.err(
                "could not parse GLSL spec: {}:{}: {s}",
                .{ diagnostic.getLine(), diagnostic.getColumn(), @errorName(err) },
            );
            std.log.err("{?s}", .{util.getJsonErrorContext(diagnostic, bytes)});
            return err;
        };
    }
};

pub const Modifier = enum {
    in,
    out,
    inout,
    @"const",
};
