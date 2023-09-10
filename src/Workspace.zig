const std = @import("std");
const lsp = @import("lsp.zig");

const Workspace = @This();

documents: std.StringHashMapUnmanaged(Document) = .{},

pub fn deinit(self: *Workspace, allocator: std.mem.Allocator) void {
    var entries = self.documents.iterator();
    while (entries.next()) |entry| {
        entry.value_ptr.deinit(allocator);
        allocator.free(entry.key_ptr.*);
    }
    self.documents.deinit(allocator);
}

pub fn getDocument(self: *Workspace, document: lsp.TextDocumentIdentifier) ?*Document {
    return self.documents.getPtr(document.uri);
}

pub fn getOrCreateDocument(
    self: *Workspace,
    allocator: std.mem.Allocator,
    document: lsp.VersionedTextDocumentIdentifier,
) !*Document {
    const entry = try self.documents.getOrPut(allocator, document.uri);
    if (!entry.found_existing) {
        errdefer self.documents.removeByPtr(entry.key_ptr);
        entry.key_ptr.* = try allocator.dupe(u8, document.uri);
        entry.value_ptr.* = .{ .version = document.version };
    }
    return entry.value_ptr;
}

pub const Document = struct {
    version: i64,

    /// The raw bytes of the file (utf-8)
    contents: std.ArrayListUnmanaged(u8) = .{},

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.contents.deinit(allocator);
    }

    pub fn replaceAll(self: *@This(), allocator: std.mem.Allocator, text: []const u8) !void {
        self.contents.shrinkRetainingCapacity(0);
        try self.contents.appendSlice(allocator, text);
    }

    pub fn replace(self: *@This(), allocator: std.mem.Allocator, range: lsp.Range, text: []const u8) !void {
        const start = self.utf8FromPosition(range.start);
        const end = self.utf8FromPosition(range.end);
        const range_len = end - start;
        try self.contents.replaceRange(allocator, start, range_len, text);
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

    pub fn wordUnderCursor(self: *@This(), cursor: lsp.Position) []const u8 {
        const offset = self.utf8FromPosition(cursor);

        var start = offset;
        var end = offset;

        const bytes = self.contents.items;
        while (start > 0 and isIdentifierChar(bytes[start - 1])) start -= 1;
        while (end < bytes.len and isIdentifierChar(bytes[end])) end += 1;

        return bytes[start..end];
    }
};

fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}
