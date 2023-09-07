const std = @import("std");

pub fn JsonEnumAsIntMixin(comptime Self: type) type {
    return struct {
        pub fn jsonStringify(self: Self, jw: anytype) !void {
            try jw.write(@intFromEnum(self));
        }

        pub fn jsonParse(
            allocator: std.mem.Allocator,
            source: anytype,
            options: std.json.ParseOptions,
        ) !Self {
            const tag = try std.json.innerParse(std.meta.Tag(Self), allocator, source, options);
            return std.meta.intToEnum(Self, tag);
        }
    };
}

pub fn getJsonErrorContext(diagnostics: std.json.Diagnostics, bytes: []const u8) []const u8 {
    const offset = diagnostics.getByteOffset();
    const start = std.mem.lastIndexOfScalar(u8, bytes[0..offset], '\n') orelse 0;
    const end = std.mem.indexOfScalarPos(u8, bytes, offset, '\n') orelse bytes.len;
    const line = bytes[@max(start, offset -| 40)..@min(end, offset +| 40)];
    return line;
}
