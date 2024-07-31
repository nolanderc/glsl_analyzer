const std = @import("std");
const lsp = @import("lsp.zig");

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
    const offset: usize = @intCast(diagnostics.getByteOffset());
    const start = std.mem.lastIndexOfScalar(u8, bytes[0..offset], '\n') orelse 0;
    const end = std.mem.indexOfScalarPos(u8, bytes, offset, '\n') orelse bytes.len;
    const line = bytes[@max(start, offset -| 40)..@min(end, offset +| 40)];
    return line;
}

pub fn normalizePath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    var buffer = try std.ArrayList(u8).initCapacity(allocator, path.len);
    errdefer buffer.deinit();

    var components = try std.fs.path.componentIterator(path);

    if (components.root()) |root| {
        try buffer.appendSlice(root);
        if (!lastIsSep(buffer.items)) try buffer.append('/');
    }

    while (components.next()) |component| {
        if (std.mem.eql(u8, component.name, "..")) {
            while (buffer.items.len > components.root_end_index) {
                if (std.fs.path.isSep(buffer.pop())) break;
            }
        } else if (std.mem.eql(u8, component.name, ".")) {
            continue;
        } else {
            if (buffer.items.len != 0 and !lastIsSep(buffer.items)) try buffer.append('/');
            try buffer.appendSlice(component.name);
        }
    }

    return buffer.toOwnedSlice();
}

fn lastIsSep(path: []const u8) bool {
    return std.fs.path.isSep(path[path.len - 1]);
}

test normalizePath {
    try expectNormalizedPath("C:/bar", "C:/blah/../bar/.");
    try expectNormalizedPath("/bar", "/../bar/.");

    if (@import("builtin").target.os.tag == .windows) {
        try expectNormalizedPath("foo/bar/baz", "foo\\bar\\baz");
    }
}

fn expectNormalizedPath(expected: []const u8, input: []const u8) !void {
    const normalized = try normalizePath(std.testing.allocator, input);
    defer std.testing.allocator.free(normalized);
    try std.testing.expectEqualStrings(expected, normalized);
}

pub fn pathFromUri(allocator: std.mem.Allocator, uri: []const u8) ![]u8 {
    const scheme = "file://";
    if (!std.mem.startsWith(u8, uri, scheme)) return error.UnknownUrlScheme;
    const uri_path = uri[scheme.len..];
    return percentDecode(allocator, uri_path);
}

pub fn uriFromPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const normalized = try normalizePath(allocator, path);
    defer allocator.free(normalized);

    return percentEncodeImpl(allocator, "file://", normalized, struct {
        fn shouldEncode(byte: u8) bool {
            return needsPercentEncode(byte) and !std.fs.path.isSep(byte);
        }
    }.shouldEncode);
}

pub fn percentEncode(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
    return percentEncodeImpl(allocator, "", text, needsPercentEncode);
}

fn percentEncodeImpl(
    allocator: std.mem.Allocator,
    comptime prefix: []const u8,
    text: []const u8,
    comptime should_encode: fn (u8) bool,
) ![]u8 {
    var encoded_characters: usize = 0;
    for (text) |byte| encoded_characters += @intFromBool(should_encode(byte));

    var out = try std.ArrayListUnmanaged(u8).initCapacity(
        allocator,
        prefix.len + text.len + 2 * encoded_characters,
    );

    out.appendSliceAssumeCapacity(prefix);

    for (text) |byte| {
        if (should_encode(byte)) {
            out.appendAssumeCapacity('%');
            out.appendSliceAssumeCapacity(&std.fmt.bytesToHex([1]u8{byte}, .upper));
        } else {
            out.appendAssumeCapacity(byte);
        }
    }

    return out.toOwnedSlice(allocator);
}

fn needsPercentEncode(byte: u8) bool {
    return switch (byte) {
        'A'...'Z',
        'a'...'z',
        '0'...'9',
        '-',
        '_',
        '.',
        '~',
        => false,
        else => true,
    };
}

pub fn percentDecode(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
    var encoded_characters: usize = 0;
    for (text) |byte| encoded_characters += @intFromBool(byte == '%');

    var out = try std.ArrayListUnmanaged(u8).initCapacity(allocator, text.len - encoded_characters * 2);

    var i: usize = 0;
    while (i < text.len) {
        if (text[i] == '%') {
            if (i + 3 > text.len) return error.Incomplete;
            const high = try parseNibble(text[i + 1]);
            const low = try parseNibble(text[i + 2]);
            const byte = (@as(u8, high) << 4) | @as(u8, low);
            out.appendAssumeCapacity(byte);
            i += 3;
        } else {
            out.appendAssumeCapacity(text[i]);
            i += 1;
        }
    }

    return out.toOwnedSlice(allocator);
}

fn parseNibble(byte: u8) !u4 {
    switch (byte) {
        '0'...'9' => return @intCast(byte - '0'),
        'A'...'F' => return @intCast(byte - 'A' + 10),
        'a'...'f' => return @intCast(byte - 'a' + 10),
        else => return error.InvalidByte,
    }
}

test "percentEncodePath" {
    try expectUriFromPath("file://C%3A/foo", "C:/foo");
}

fn expectUriFromPath(expected: []const u8, input: []const u8) !void {
    const encoded = try uriFromPath(std.testing.allocator, input);
    defer std.testing.allocator.free(encoded);
    try std.testing.expectEqualStrings(expected, encoded);
}

test "percentEncode" {
    try expectPercentEncoded("C%3A", "C:");
}

fn expectPercentEncoded(expected: []const u8, input: []const u8) !void {
    const encoded = try percentEncode(std.testing.allocator, input);
    defer std.testing.allocator.free(encoded);
    try std.testing.expectEqualStrings(expected, encoded);
}

test "percentDecode" {
    try expectPercentDecoded("C:", "C%3A");
}

fn expectPercentDecoded(expected: []const u8, input: []const u8) !void {
    const decoded = try percentDecode(std.testing.allocator, input);
    defer std.testing.allocator.free(decoded);
    try std.testing.expectEqualStrings(expected, decoded);
}

pub fn positionFromUtf8(text: []const u8, offset: u32) lsp.Position {
    var line_breaks: u32 = 0;
    var line_start: usize = 0;

    const before = text[0..offset];

    for (before, 0..) |ch, index| {
        if (ch == '\n') {
            line_breaks += 1;
            line_start = index + 1;
        }
    }

    const last_line = before[line_start..];
    const character = std.unicode.calcUtf16LeLen(last_line) catch last_line.len;

    return .{ .line = line_breaks, .character = @intCast(character) };
}
