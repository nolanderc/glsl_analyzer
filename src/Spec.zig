const std = @import("std");
const util = @import("util.zig");

comment: []const u8 = "",
keywords: []const Keyword,
operators: []const Operator,
types: []const Type,
variables: []const Variable,
functions: []const Function,

pub const Keyword = struct {
    name: []const u8,
    kind: Kind,
    pub const Kind = enum { glsl, vulkan, reserved };
};

pub const Operator = struct {
    name: []const u8,
    precedence: u8,
    left_to_right: bool,
    kind: Kind,
    pub const Kind = enum { prefix, infix, postfix };
};

pub const Type = struct {
    name: []const u8,
    description: []const []const u8,
};

pub const Variable = struct {
    modifiers: Modifiers = .{ .in = true },
    type: []const u8,
    name: []const u8,
    default_value: ?[]const u8 = null,
    description: ?[]const []const u8 = null,
    versions: ?[]const u16 = null,
    extensions: ?[]const []const u8 = null,
};

pub const Function = struct {
    return_type: []const u8,
    name: []const u8,
    parameters: []const Parameter,
    description: ?[]const []const u8 = null,
    versions: ?[]const u16 = null,
    extensions: ?[]const []const u8 = null,

    pub const Parameter = struct {
        modifiers: ?Modifiers = null,
        type: []const u8,
        name: []const u8,
        optional: bool = false,
    };
};

const spec_bytes = @embedFile("glsl_spec.json");

pub fn load(allocator: std.mem.Allocator) !@This() {
    var diagnostic = std.json.Diagnostics{};
    var scanner = std.json.Scanner.initCompleteInput(allocator, spec_bytes);
    defer scanner.deinit();
    scanner.enableDiagnostics(&diagnostic);

    return std.json.parseFromTokenSourceLeaky(@This(), allocator, &scanner, .{}) catch |err| {
        std.log.err(
            "could not parse GLSL spec: {}:{}: {s}",
            .{ diagnostic.getLine(), diagnostic.getColumn(), @errorName(err) },
        );
        std.log.err("{s}", .{util.getJsonErrorContext(diagnostic, spec_bytes)});
        return err;
    };
}

pub const Modifiers = packed struct(u3) {
    in: bool = false,
    out: bool = false,
    @"const": bool = false,

    pub fn jsonParse(
        allocator: std.mem.Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) !@This() {
        const text = try std.json.innerParse([]const u8, allocator, source, options);
        var words = std.mem.splitScalar(u8, text, ' ');

        var modifiers = Modifiers{};
        while (words.next()) |word| {
            inline for (comptime std.meta.fieldNames(@This())) |name| {
                if (std.mem.eql(u8, word, name)) {
                    if (@field(modifiers, name)) return error.DuplicateField;
                    @field(modifiers, name) = true;
                }
            }
        }
        return modifiers;
    }

    const buffer_len = blk: {
        var max_len: usize = std.meta.fieldNames(@This()).len;
        for (std.meta.fieldNames(@This())) |name| max_len += name.len;
        break :blk max_len;
    };

    const FormatBuffer = std.ArrayListUnmanaged(u8);

    fn toString(self: @This(), buffer: *FormatBuffer) void {
        inline for (comptime std.meta.fieldNames(@This())) |name| {
            if (@field(self, name)) {
                if (buffer.items.len != 0) buffer.appendAssumeCapacity(' ');
                buffer.appendSliceAssumeCapacity(name);
            }
        }
    }

    pub fn jsonStringify(self: @This(), jw: anytype) !void {
        var buffer: [buffer_len]u8 = undefined;
        var format_buffer = FormatBuffer.initBuffer(&buffer);
        self.toString(&format_buffer);
        try jw.write(format_buffer.items);
    }

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        var buffer: [buffer_len]u8 = undefined;
        var format_buffer = FormatBuffer.initBuffer(&buffer);
        self.toString(&format_buffer);
        try writer.writeAll(format_buffer.items);
    }
};
