const std = @import("std");
const lsp = @import("lsp.zig");
const parse = @import("parse.zig");

allocator: std.mem.Allocator,

version: ?i64,

/// The raw bytes of the file (utf-8)
contents: std.ArrayListUnmanaged(u8) = .{},

/// Parse tree of the document (computed on-demand)
parse_tree: ?CompleteParseTree = null,

pub fn deinit(self: *@This()) void {
    self.contents.deinit(self.allocator);
    if (self.parse_tree) |*tree| tree.deinit(self.allocator);
}

pub fn invalidate(self: *@This()) void {
    if (self.parse_tree) |*tree| tree.deinit(self.allocator);
    self.parse_tree = null;
}

pub fn replaceAll(self: *@This(), text: []const u8) !void {
    self.invalidate();
    self.contents.shrinkRetainingCapacity(0);
    try self.contents.appendSlice(self.allocator, text);
}

pub fn replace(self: *@This(), range: lsp.Range, text: []const u8) !void {
    self.invalidate();
    const start = self.utf8FromPosition(range.start);
    const end = self.utf8FromPosition(range.end);
    const range_len = end - start;
    try self.contents.replaceRange(self.allocator, start, range_len, text);
}

pub fn utf8FromPosition(self: @This(), position: lsp.Position) u32 {
    var remaining_lines = position.line;
    var i: usize = 0;
    const bytes = self.contents.items;

    while (remaining_lines != 0 and i < bytes.len) {
        remaining_lines -= @intFromBool(bytes[i] == '\n');
        i += 1;
    }

    const rest = self.contents.items[i..];

    var remaining_chars = position.character;
    var codepoints = std.unicode.Utf8View.initUnchecked(rest).iterator();
    while (remaining_chars != 0) {
        const codepoint = codepoints.nextCodepoint() orelse break;
        remaining_chars -|= std.unicode.utf16CodepointSequenceLength(codepoint) catch unreachable;
    }

    return @intCast(i + codepoints.i);
}

fn getByte(self: @This(), index: usize) u8 {
    return self.contents.items[index];
}

pub fn wholeRange(self: @This()) lsp.Range {
    var line_breaks: u32 = 0;
    var line_start: usize = 0;

    for (self.contents.items, 0..) |ch, index| {
        if (ch == '\n') {
            line_breaks += 1;
            line_start = index + 1;
        }
    }

    const last_line = self.contents.items[line_start..];
    const character = std.unicode.calcUtf16LeLen(last_line) catch last_line.len;

    return .{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = line_breaks, .character = @intCast(character) },
    };
}

pub fn wordUnderCursor(self: *@This(), cursor: lsp.Position) []const u8 {
    const offset = self.utf8FromPosition(cursor);

    var start = offset;
    var end = offset;

    const bytes = self.contents.items;
    while (start > 0 and isIdentifierChar(bytes[start - 1])) start -= 1;
    while (end < bytes.len and isIdentifierChar(bytes[end])) end += 1;

    return bytes[start..end];
}

fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

pub fn parseTree(self: *@This()) !*const CompleteParseTree {
    if (self.parse_tree) |*tree| return tree;
    self.parse_tree = try CompleteParseTree.parseSource(self.allocator, self.contents.items);
    return &self.parse_tree.?;
}

pub const CompleteParseTree = struct {
    raw: parse.Tree,
    ignored: std.ArrayListUnmanaged(parse.Token),
    diagnostics: std.ArrayListUnmanaged(parse.Diagnostic),

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.raw.deinit(allocator);
        self.ignored.deinit(allocator);
        self.diagnostics.deinit(allocator);
    }

    pub fn parseSource(allocator: std.mem.Allocator, source: []const u8) !@This() {
        var diagnostics = std.ArrayList(parse.Diagnostic).init(allocator);
        errdefer diagnostics.deinit();

        var ignored = std.ArrayList(parse.Token).init(allocator);
        errdefer ignored.deinit();

        const tree = try parse.parse(allocator, source, .{
            .ignored = &ignored,
            .diagnostics = &diagnostics,
        });

        return .{
            .raw = tree,
            .ignored = ignored.moveToUnmanaged(),
            .diagnostics = diagnostics.moveToUnmanaged(),
        };
    }
};
