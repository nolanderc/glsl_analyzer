const std = @import("std");

pub const Node = union(enum) {
    pub const Tag = std.meta.Tag(Node);

    pub const Range = struct {
        start: u32,
        end: u32,
    };

    eof: void,

    unknown: Token,

    comment: Token,

    identifier: Token,
    number: Token,
    string: Token,

    keyword: KeywordToken,
    symbol: SymbolToken,

    invalid: Range,
};

pub const Tree = struct {
    nodes: std.MultiArrayList(Node) = .{},
};

pub const Token = struct {
    start: u32,
    end: u32,
};

pub const KeywordToken = struct {
    kind: Keyword,
    start: u32,
};

pub const Keyword = enum {
    @"const",
    uniform,
    buffer,
    shared,
    attribute,
    varying,

    coherent,
    @"volatile",
    restrict,
    readonly,
    writeonly,

    layout,
    centroid,
    flat,
    smooth,
    noperspective,

    patch,
    sample,

    invariant,
    precise,

    @"break",
    @"continue",
    do,
    @"for",
    @"while",
    @"switch",
    case,
    default,
    @"if",
    @"else",

    subroutine,

    in,
    out,
    inout,

    discard,
    @"return",

    true,
    false,

    lowp,
    mediump,
    highp,
    precision,

    @"struct",

    pub fn len(self: Keyword) u8 {
        return switch (self) {
            inline else => |this| @tagName(this).len,
        };
    }
};

pub const SymbolToken = struct {
    kind: Symbol,
    start: u32,
};

pub const Symbol = enum {
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",

    @".",
    @":",
    @",",
    @";",

    @"?",

    @"!",
    @"~",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",
    @"&",
    @"|",
    @"^",
    @"<<",
    @">>",

    @"=",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @"%=",
    @"&=",
    @"|=",
    @"^=",
    @"<<=",
    @">>=",

    @"==",
    @"!=",
    @"<",
    @"<=",
    @">",
    @">=",

    @"&&",
    @"||",
    @"^^",

    @"++",
    @"--",

    pub fn len(self: Symbol) u8 {
        return switch (self) {
            inline else => |this| @tagName(this).len,
        };
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokenizer: Tokenizer,
    next_token: Node,

    tree: Tree = .{},
    stack: std.ArrayListUnmanaged(Node) = .{},

    pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
        return .{
            .allocator = allocator,
            .tokenizer = Tokenizer{ .source = source },
            .next_token = undefined,
        };
    }

    pub fn advance(self: *@This()) void {
        while (true) {
            const token = self.tokenizer.next();
            if (token == .comment) continue;
            self.next_token = token;
            return;
        }
    }
};

pub const Tokenizer = struct {
    source: []const u8,
    offset: u32 = 0,

    pub fn next(self: *@This()) Node {
        const text = self.source;
        const N = text.len;

        var i = self.offset;

        // skip whitespace
        while (i < N and std.ascii.isWhitespace(text[i])) i += 1;
        if (i == N) return .eof;

        self.offset = i;

        switch (text[i]) {
            '0'...'9' => {
                i += 1;
                while (i < N and isIdentifierChar(text[i])) i += 1;
                return .{ .number = self.token(i) };
            },

            'a'...'z', 'A'...'Z', '_' => {
                i += 1;
                while (i < N and isIdentifierChar(text[i])) i += 1;
                const ident = self.token(i);
                return mapKeyword(self.source[ident.start..ident.end], ident);
            },

            else => {
                i += std.unicode.utf8ByteSequenceLength(text[i]) catch 1;
                if (i > N) i = @intCast(N);
                return .{ .unknown = self.token(i) };
            },
        }
    }

    fn token(self: *@This(), end: u32) Token {
        const start = self.offset;
        self.offset = end;
        return .{ .start = start, .end = end };
    }

    fn isIdentifierChar(c: u8) bool {
        switch (c) {
            '_',
            'a'...'z',
            'A'...'Z',
            '0'...'9',
            => return true,
            else => return false,
        }
    }

    fn mapKeyword(name: []const u8, span: Token) Node {
        const map = comptime blk: {
            const keywords = std.meta.tags(Keyword);
            var table: [keywords.len]struct { []const u8, Keyword } = undefined;
            for (&table, keywords) |*entry, keyword| entry.* = .{ @tagName(keyword), keyword };
            break :blk std.ComptimeStringMap(Keyword, table);
        };

        if (map.get(name)) |keyword| {
            return .{ .keyword = .{ .start = span.start, .kind = keyword } };
        } else {
            return .{ .identifier = span };
        }
    }
};

test {
    std.testing.refAllDeclsRecursive(@This());
}
