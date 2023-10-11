const std = @import("std");
const lsp = @import("lsp.zig");
const parse = @import("parse.zig");
const Workspace = @import("Workspace.zig");

workspace: *Workspace,

uri: []const u8,
version: ?i64,

/// The raw bytes of the file (utf-8)
contents: std.ArrayListUnmanaged(u8) = .{},

/// Parse tree of the document (computed on-demand)
parse_tree: ?CompleteParseTree = null,

pub fn deinit(self: *@This()) void {
    self.contents.deinit(self.workspace.allocator);
    if (self.parse_tree) |*tree| tree.deinit(self.workspace.allocator);
}

pub fn invalidate(self: *@This()) void {
    if (self.parse_tree) |*tree| tree.deinit(self.workspace.allocator);
    self.parse_tree = null;
}

pub fn source(self: @This()) []const u8 {
    return self.contents.items;
}

pub fn replaceAll(self: *@This(), text: []const u8) !void {
    self.invalidate();
    self.contents.shrinkRetainingCapacity(0);
    try self.contents.appendSlice(self.workspace.allocator, text);
}

pub fn replace(self: *@This(), range: lsp.Range, text: []const u8) !void {
    self.invalidate();
    const start = self.utf8FromPosition(range.start);
    const end = self.utf8FromPosition(range.end);
    const range_len = end - start;
    try self.contents.replaceRange(self.workspace.allocator, start, range_len, text);
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

pub fn wholeRange(self: @This()) lsp.Range {
    return .{
        .start = .{ .line = 0, .character = 0 },
        .end = positionFromUtf8(self.source(), @intCast(self.contents.items.len)),
    };
}

pub fn nodeRange(self: *@This(), node: u32) !lsp.Range {
    const parsed = try self.parseTree();
    const span = parsed.tree.nodeSpan(node);
    return .{
        .start = positionFromUtf8(self.source(), span.start),
        .end = positionFromUtf8(self.source(), span.end),
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

/// Return the node right under the cursor.
pub fn nodeUnderCursor(self: *@This(), cursor: lsp.Position) !?u32 {
    const offset = self.utf8FromPosition(cursor);
    const parsed = try self.parseTree();
    const tree = parsed.tree;

    for (0.., tree.nodes.items(.tag), tree.nodes.items(.span)) |index, tag, span| {
        if (tag.isToken() and span.start <= offset and offset < span.end) {
            return @intCast(index);
        }
    }

    return null;
}

/// Return the node closest to left of the cursor.
pub fn nodeBeforeCursor(self: *@This(), cursor: lsp.Position) !?u32 {
    const offset = self.utf8FromPosition(cursor);
    const parsed = try self.parseTree();
    const tree = parsed.tree;

    var best: ?u32 = null;
    var best_end: u32 = 0;

    for (0.., tree.nodes.items(.tag), tree.nodes.items(.span)) |index, tag, span| {
        if (tag.isToken()) {
            // ignore empty tokens
            if (span.start == span.end) continue;

            // ignore tokens after the cursor
            if (offset < span.start) continue;

            if (span.end > best_end) {
                // found a token further to the right
                best = @intCast(index);
                best_end = span.end;
            }
        }
    }

    return best;
}

fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

pub fn parseTree(self: *@This()) !*const CompleteParseTree {
    if (self.parse_tree) |*tree| return tree;
    self.parse_tree = try CompleteParseTree.parseSource(
        self.workspace.allocator,
        self.contents.items,
    );
    return &self.parse_tree.?;
}

pub const CompleteParseTree = struct {
    arena_state: std.heap.ArenaAllocator.State,
    tree: parse.Tree,
    ignored: []parse.Token,
    diagnostics: std.ArrayListUnmanaged(parse.Diagnostic),

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.arena_state.promote(allocator).deinit();
    }

    pub fn parseSource(parent_allocator: std.mem.Allocator, text: []const u8) !@This() {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        errdefer arena.deinit();

        const allocator = arena.allocator();

        var diagnostics = std.ArrayList(parse.Diagnostic).init(allocator);
        var ignored = std.ArrayList(parse.Token).init(parent_allocator);
        defer ignored.deinit();

        const tree = try parse.parse(allocator, text, .{
            .ignored = &ignored,
            .diagnostics = &diagnostics,
        });

        return .{
            .arena_state = arena.state,
            .tree = tree,
            .ignored = tree.ignored(),
            .diagnostics = diagnostics.moveToUnmanaged(),
        };
    }
};
