const std = @import("std");
const lsp = @import("lsp.zig");

pub fn Message(comptime Inner: type) type {
    return union(enum) {
        single: Inner,
        batch: []Inner,

        pub fn jsonParse(
            allocator: std.mem.Allocator,
            source: anytype,
            options: std.json.ParseOptions,
        ) !@This() {
            switch (try source.peekNextTokenType()) {
                .object_begin => return .{
                    .single = try std.json.innerParse(Inner, allocator, source, options),
                },
                .array_begin => return .{
                    .batch = try std.json.innerParse([]Inner, allocator, source, options),
                },
                else => return error.UnexpectedToken,
            }
        }
    };
}

pub const Request = struct {
    pub const Id = std.json.Value;

    jsonrpc: []const u8,
    method: []const u8,
    id: Id = .null,
    params: std.json.Value = .null,
};

pub const Response = struct {
    jsonrpc: []const u8 = "2.0",
    id: Request.Id,
    result: Result,

    pub const Result = union(enum) {
        success: JsonPreformatted,
        failure: lsp.Error,
    };

    pub fn jsonStringify(self: @This(), jw: anytype) !void {
        try jw.beginObject();

        try jw.objectField("jsonrpc");
        try jw.write(self.jsonrpc);

        try jw.objectField("id");
        try jw.write(self.id);

        switch (self.result) {
            .success => |data| {
                try jw.objectField("result");
                try jw.write(data);
            },
            .failure => |err| {
                try jw.objectField("error");
                try jw.write(err);
            },
        }

        try jw.endObject();
    }
};

pub const JsonPreformatted = struct {
    raw: []const u8,

    pub fn jsonStringify(self: @This(), jw: anytype) !void {
        try jw.print("{s}", .{self.raw});
    }
};
