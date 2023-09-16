const std = @import("std");

pub const Node = union(enum) {
    pub const Tag = std.meta.Tag(Node);

    pub const Range = struct {
        start: u32,
        end: u32,
    };

    eof: Token,

    unknown: Token,

    comment: Token,

    // a preprocessor directive: includes everyting from the leading `#` until
    // the next line ending. May contain comments and line continuations (`\\\n`).
    preprocessor: Token,

    identifier: Token,
    number: Token,
    string: Token,

    keyword_const: Token,
    keyword_uniform: Token,
    keyword_buffer: Token,
    keyword_shared: Token,
    keyword_attribute: Token,
    keyword_varying: Token,

    keyword_coherent: Token,
    keyword_volatile: Token,
    keyword_restrict: Token,
    keyword_readonly: Token,
    keyword_writeonly: Token,

    keyword_layout: Token,
    keyword_centroid: Token,
    keyword_flat: Token,
    keyword_smooth: Token,
    keyword_noperspective: Token,

    keyword_patch: Token,
    keyword_sample: Token,

    keyword_invariant: Token,
    keyword_precise: Token,

    keyword_break: Token,
    keyword_continue: Token,
    keyword_do: Token,
    keyword_for: Token,
    keyword_while: Token,
    keyword_switch: Token,
    keyword_case: Token,
    keyword_default: Token,
    keyword_if: Token,
    keyword_else: Token,

    keyword_subroutine: Token,

    keyword_in: Token,
    keyword_out: Token,
    keyword_inout: Token,

    keyword_discard: Token,
    keyword_return: Token,

    keyword_true: Token,
    keyword_false: Token,

    keyword_lowp: Token,
    keyword_mediump: Token,
    keyword_highp: Token,
    keyword_precision: Token,

    keyword_struct: Token,

    @"(": Token,
    @")": Token,
    @"[": Token,
    @"]": Token,
    @"{": Token,
    @"}": Token,

    @".": Token,
    @",": Token,
    @":": Token,
    @";": Token,

    @"?": Token,

    @"~": Token,
    @"!": Token,

    @"+": Token,
    @"-": Token,
    @"*": Token,
    @"/": Token,
    @"%": Token,
    @"&": Token,
    @"|": Token,
    @"^": Token,
    @"<<": Token,
    @">>": Token,

    @"=": Token,
    @"+=": Token,
    @"-=": Token,
    @"*=": Token,
    @"/=": Token,
    @"%=": Token,
    @"&=": Token,
    @"|=": Token,
    @"^=": Token,
    @"<<=": Token,
    @">>=": Token,

    @"==": Token,
    @"!=": Token,
    @"<": Token,
    @"<=": Token,
    @">": Token,
    @">=": Token,

    @"&&": Token,
    @"||": Token,
    @"^^": Token,

    @"++": Token,
    @"--": Token,

    invalid: Range,
    subroutine: Range,
    parenthized: Range,
    expression_sequence: Range,
    assignment: Range,
    conditional: Range,
    unary: Range,

    pub fn initToken(tag: Tag, token: Token) Node {
        switch (tag) {
            inline else => |constant| {
                const Payload = @TypeOf(@field(@as(Node, undefined), @tagName(constant)));
                if (Payload == Token) {
                    return @unionInit(Node, @tagName(constant), token);
                } else {
                    unreachable;
                }
            },
        }
    }

    pub fn getToken(self: @This()) ?Token {
        switch (self) {
            inline else => |payload| {
                return if (@TypeOf(payload) == Token) payload else null;
            },
        }
    }
};

pub const Tree = struct {
    nodes: std.MultiArrayList(Node) = .{},
};

pub const Token = struct {
    start: u32,
    end: u32,
};

const TokenSet = std.EnumSet(Node.Tag);

pub const Span = Token;

pub const Diagnostic = struct {
    span: Span,
    message: []const u8,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokenizer: Tokenizer,
    next: Node,
    fuel: u32 = max_fuel,

    deferred_error: ?Error = null,

    tree: Tree = .{},
    stack: std.MultiArrayList(Node) = .{},

    diagnostics: ?*std.ArrayList(Diagnostic) = null,

    const max_fuel = 32;

    pub const Error = error{OutOfMemory};

    pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
        return .{
            .allocator = allocator,
            .tokenizer = Tokenizer{ .source = source },
            .next = undefined,
        };
    }

    fn deferError(self: *@This(), value: anytype) @typeInfo(@TypeOf(value)).ErrorUnion.payload {
        if (value) |success| {
            return success;
        } else |err| {
            if (self.deferred_error == null) {
                self.deferred_error = err;
            }
        }
    }

    fn advance(self: *@This()) void {
        self.deferError(self.stack.append(self.allocator, self.next));

        while (true) {
            const token = self.tokenizer.next();
            if (token == .comment or token == .preprocessor) continue;
            self.next = token;
            self.fuel = max_fuel;
            return;
        }
    }

    fn peek(self: *@This()) Node.Tag {
        self.fuel -= 1;
        if (self.fuel == 0) std.debug.panic("ran out of fuel", .{});
        return self.next;
    }

    fn at(self: *@This(), expected: Node.Tag) bool {
        return self.peek() == expected;
    }

    fn atAny(self: *@This(), set: TokenSet) bool {
        return set.contains(self.peek());
    }

    fn eof(self: *@This()) bool {
        return self.at(.eof);
    }

    fn eat(self: *@This(), expected: Node.Tag) bool {
        const found = self.at(expected);
        if (found) self.advance();
        return found;
    }

    fn expect(self: *@This(), comptime expected: Node.Tag) void {
        if (!self.eat(expected)) {
            self.emitError("expected " ++ @tagName(expected));
        }
    }

    fn emitError(self: *@This(), message: []const u8) void {
        if (self.diagnostics) |diagnostics| {
            self.deferError(diagnostics.append(.{
                .span = self.next.getToken() orelse unreachable,
                .message = message,
            }));
        }
    }

    fn advanceWithError(self: *@This(), message: []const u8) void {
        self.emitError(message);
        self.advance();
    }

    const Mark = struct {
        index: usize,

        fn isEmpty(mark: @This(), p: *Parser) bool {
            return mark.index == p.stack.len;
        }
    };

    fn open(self: *@This()) Mark {
        return .{ .index = self.stack.len };
    }

    fn closeRange(self: *@This(), mark: Mark) Node.Range {
        const tags = self.stack.items(.tags)[mark.index..];
        const data = self.stack.items(.data)[mark.index..];

        const start = self.tree.nodes.len;
        const end = start + tags.len;

        self.deferError(self.tree.nodes.ensureUnusedCapacity(self.allocator, tags.len));
        @memcpy(self.tree.nodes.items(.tags)[start..], tags);
        @memcpy(self.tree.nodes.items(.data)[start..], data);
        self.tree.nodes.len += tags.len;

        self.stack.len = mark.index;

        return Node.Range{
            .start = @intCast(start),
            .end = @intCast(end),
        };
    }

    fn close(self: *@This(), mark: Mark, comptime kind: Node.Tag) void {
        const range = self.closeRange(mark);
        self.deferError(self.stack.append(self.allocator, @unionInit(Node, @tagName(kind), range)));
    }
};

pub fn parseFile(p: *Parser) void {
    while (!p.eof()) {
        if (p.atAny(external_declaration_first)) {
            externalDeclaration(p);
        } else {
            p.advanceWithError("expected a declaration");
        }
    }
}

const external_declaration_first = TokenSet.initMany(&.{.@";"});

fn externalDeclaration(p: *Parser) void {
    if (p.eat(.@";")) return;
}

const type_qualifier_first = TokenSet.initMany(&.{});

fn type_qualifier(p: *Parser) void {
    switch (p.peek()) {
        // storage_qualifier
        .keyword_const,
        .keyword_in,
        .keyword_out,
        .keyword_inout,
        .keyword_centroid,
        .keyword_patch,
        .keyword_sample,
        .keyword_uniform,
        .keyword_buffer,
        .keyword_shared,
        .keyword_coherent,
        .keyword_volatile,
        .keyword_restrict,
        .keyword_readonly,
        .keyword_writeonly,
        => p.advance(),

        // layout_qualifier
        .keyword_layout => {
            p.advance();
            p.expect(.@"(");
            while (true) {
                if (p.eat(.identifier)) {
                    p.expect(.@"=");
                    constantExpression(p);
                } else {
                    p.expect(.keyword_shared);
                }
                if (p.at(.@")")) break else p.expect(.@",");
            }
            p.expect(.@")");
        },

        // precision_qualifier
        // interpolation_qualifier
        // invariant_qualifier
        // precise_qualifier

        .keyword_subroutine => {
            const m = p.open();
            p.advance();
            if (p.eat(.@"(")) {
                while (p.eat(.identifier)) {
                    if (!p.at(.@")")) p.expect(.@",");
                }
                p.expect(.@")");
            }
            p.close(m, .subroutine);
        },
        else => return,
    }
}

fn expression(p: *Parser) void {
    const m = p.open();
    assignmentExpression(p);
    while (p.eat(.@",")) {
        assignmentExpression(p);
        p.close(m, .expression_sequence);
    }
}

const assignment_operator = TokenSet.initMany(&.{
    .@"=",
    .@"+=",
    .@"-=",
    .@"*=",
    .@"/=",
    .@"%=",
    .@"<<=",
    .@">>=",
    .@"&=",
    .@"^=",
    .@"|=",
});

fn assignmentExpression(p: *Parser) void {
    const m = p.open();
    constantExpression();
    while (p.atAny(assignment_operator)) {
        p.advance();
        assignmentExpression(p);
        p.close(m, .assignment);
    }
}

fn constantExpression(p: *Parser) void {
    const m = p.open();
    if (!primaryExpressionOpt(p)) p.emitError("expected an expression");
    if (p.eat(.@"?")) {
        expression(p);
        p.expect(.@":");
        assignmentExpression(p);
        p.close(m, .conditional);
    }
}

fn infixExpressionOpt(p: *Parser) bool {
    if (!unaryExpressionOpt(p)) return false;
}

const unary_operators = TokenSet.initMany(&.{
    .@"+",
    .@"-",
    .@"!",
    .@"~",
    .@"++",
    .@"--",
});

fn unaryExpressionOpt(p: *Parser) bool {
    const m = p.open();
    var has_operator = false;
    while (p.atAny(unary_operators)) {
        p.advance();
        has_operator = true;
    }

    const has_expression = postfixExpressionOpt(p);

    if (!has_expression and has_operator) p.emitError("expected expression");
    if (has_operator) p.close(m, .unary);

    return has_operator or has_expression;
}

const postfix_operators = TokenSet.initMany(&.{
    .@"[",
    .@"(",
    .@".",
    .@"++",
    .@"--",
});

fn postfixExpressionOpt(p: *Parser) bool {
    const m = p.open();
    if (!primaryExpressionOpt(p)) return false;

    while (true) {
        switch (p.peek()) {
            .@"[" => {
                p.advance();
                expression(p);
                p.expect(.@"]");
                p.close(m, .index);
            },
            .@"(" => {
                p.advance();
                expression(p);
                p.expect(.@")");
                p.close(m, .call);
            },
            else => break,
        }
        if (p.eat(.@"[")) {
            continue;
        }
    }
}

fn primaryExpressionOpt(p: *Parser) bool {
    switch (p.peek()) {
        .identifier, .number, .keyword_true, .keyword_false => {
            p.advance();
            return true;
        },
        .@"(" => {
            const m = p.open();
            p.advance();
            expression(p);
            p.expect(.@")");
            p.close(m, .parenthized);
            return true;
        },
        else => return false,
    }
}

pub const Tokenizer = struct {
    source: []const u8,
    offset: u32 = 0,

    pub fn nextText(self: *@This()) ?[]const u8 {
        const node = self.next();
        const span = node.getToken() orelse return null;
        return self.source[span.start..span.end];
    }

    pub fn next(self: *@This()) Node {
        const text = self.source;
        const N = text.len;

        var i = self.offset;

        // skip whitespace
        while (i < N and std.ascii.isWhitespace(text[i])) i += 1;
        self.offset = i;

        if (i == N) return self.token(.eof, i);

        const first = text[i];
        switch (first) {
            '0'...'9', '.' => {
                if (text[i] == '0') {
                    i += 1;
                    if (i < N and (text[i] == 'x' or text[i] == 'X')) {
                        // hexadecimal
                        i += 1;
                        while (i < N and std.ascii.isHex(text[i])) i += 1;
                    }
                }

                // decimal/octal
                while (i < N and std.ascii.isDigit(text[i])) i += 1;

                if (i < N and (text[i] == 'u' or text[i] == 'U')) {
                    // unsigned (cannot be float)
                    i += 1;
                    return self.token(.number, i);
                }

                if (i < N and text[i] == '.') {
                    // fractional part
                    i += 1;
                    while (i < N and std.ascii.isDigit(text[i])) i += 1;
                }

                if (i < N and (text[i] == 'e' or text[i] == 'E')) {
                    // exponent
                    i += 1;
                    if (i < N and (text[i] == '+' or text[i] == '-')) i += 1;
                    while (i < N and std.ascii.isDigit(text[i])) i += 1;
                }

                if (i < N and (text[i] == 'f' or text[i] == 'F')) {
                    i += 1;
                } else if (i + 1 < N and (std.mem.startsWith(u8, text[i..], "lf") or
                    std.mem.startsWith(u8, text[i..], "LF")))
                {
                    i += 1;
                }

                if (first == '.' and self.offset + 1 == i) {
                    return self.token(.@".", i);
                } else {
                    return self.token(.number, i);
                }
            },

            'a'...'z', 'A'...'Z', '_' => {
                i += 1;
                while (i < N and isIdentifierChar(text[i])) i += 1;
                const ident = self.tokenSpan(i);
                return mapKeyword(self.source[ident.start..ident.end], ident);
            },

            '"' => {
                i += 1;
                while (i < N) {
                    if (text[i] == '"' or text[i] == '\n') {
                        i += 1;
                        break;
                    }
                    if (text[i] == '\\') {
                        i +|= 2;
                        if (i > N) i = @intCast(N);
                        continue;
                    }
                    i += 1;
                }

                return self.token(.string, i);
            },

            '(' => return self.token(.@"(", i + 1),
            ')' => return self.token(.@")", i + 1),
            '[' => return self.token(.@"[", i + 1),
            ']' => return self.token(.@"]", i + 1),
            '{' => return self.token(.@"{", i + 1),
            '}' => return self.token(.@"}", i + 1),

            ',' => return self.token(.@",", i + 1),
            ':' => return self.token(.@":", i + 1),
            ';' => return self.token(.@";", i + 1),

            '~' => return self.token(.@"~", i + 1),
            '?' => return self.token(.@"?", i + 1),

            '!' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"!=", i + 2),
                else => return self.token(.@"!", i + 1),
            },

            '+' => switch (getOrZero(text, i + 1)) {
                '+' => return self.token(.@"++", i + 2),
                '=' => return self.token(.@"+=", i + 2),
                else => return self.token(.@"+", i + 1),
            },
            '-' => switch (getOrZero(text, i + 1)) {
                '-' => return self.token(.@"--", i + 2),
                '=' => return self.token(.@"-=", i + 2),
                else => return self.token(.@"-", i + 1),
            },
            '*' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"*=", i + 2),
                else => return self.token(.@"*", i + 1),
            },
            '/' => switch (getOrZero(text, i + 1)) {
                '/' => {
                    while (i < text.len and text[i] != '\n') i += 1;
                    return self.token(.comment, i);
                },
                '*' => {
                    while (i + 1 < text.len and text[i] != '*' and text[i + 1] != '/') i += 1;
                    return self.token(.comment, i);
                },
                '=' => return self.token(.@"/=", i + 2),
                else => return self.token(.@"/", i + 1),
            },
            '%' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"&=", i + 2),
                '&' => return self.token(.@"&&", i + 2),
                else => return self.token(.@"&", i + 1),
            },
            '|' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"|=", i + 2),
                '|' => return self.token(.@"||", i + 2),
                else => return self.token(.@"|", i + 1),
            },
            '^' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"^=", i + 2),
                '^' => return self.token(.@"^^", i + 2),
                else => return self.token(.@"^", i + 1),
            },

            '=' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"==", i + 2),
                else => return self.token(.@"=", i + 1),
            },

            '<' => switch (getOrZero(text, i + 1)) {
                '<' => switch (getOrZero(text, i + 2)) {
                    '=' => return self.token(.@"<<=", i + 3),
                    else => return self.token(.@"<<", i + 2),
                },
                '=' => return self.token(.@"<=", i + 2),
                else => return self.token(.@"<", i + 1),
            },
            '>' => switch (getOrZero(text, i + 1)) {
                '>' => switch (getOrZero(text, i + 2)) {
                    '=' => return self.token(.@">>=", i + 3),
                    else => return self.token(.@">>", i + 2),
                },
                '=' => return self.token(.@">=", i + 2),
                else => return self.token(.@">", i + 1),
            },

            '#' => {
                while (i < N and text[i] != '\n') {
                    if (text[i] == '\\') {
                        i += 1;
                        if (i < N) i += 1;
                        continue;
                    }
                    i += 1;
                }

                return self.token(.preprocessor, i);
            },

            else => {
                comptime var valid_char = std.StaticBitSet(256).initEmpty();
                comptime {
                    for (std.ascii.whitespace) |c| valid_char.set(c);
                    for ('0'..'9' + 1) |c| valid_char.set(c);
                    for ('a'..'z' + 1) |c| valid_char.set(c);
                    for ('A'..'Z' + 1) |c| valid_char.set(c);
                    valid_char.set('_');
                    valid_char.set('"');
                    valid_char.set('#');
                    for (std.meta.tags(Node.Tag)) |tag| {
                        const name = @tagName(tag);
                        valid_char.set(name[0]);
                    }
                }

                while (i < N and !valid_char.isSet(text[i])) i += 1;
                std.debug.assert(i != self.offset);

                return self.token(.unknown, i);
            },
        }
    }

    fn getOrZero(text: []const u8, index: u32) u8 {
        return if (index < text.len) text[index] else 0;
    }

    fn token(self: *@This(), comptime kind: Node.Tag, end: u32) Node {
        return @unionInit(Node, @tagName(kind), self.tokenSpan(end));
    }

    fn tokenSpan(self: *@This(), end: u32) Token {
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

    fn mapKeyword(identifier: []const u8, span: Token) Node {
        const map = comptime blk: {
            @setEvalBranchQuota(2000);

            const tags = std.meta.tags(Node.Tag);

            var table = std.BoundedArray(struct { []const u8, Node.Tag }, tags.len){};

            for (tags) |tag| {
                if (stripPrefix(@tagName(tag), "keyword_")) |name| {
                    table.appendAssumeCapacity(.{ name, tag });
                }
            }

            break :blk std.ComptimeStringMap(Node.Tag, table.slice());
        };

        if (map.get(identifier)) |tag| {
            return Node.initToken(tag, span);
        } else {
            return .{ .identifier = span };
        }
    }
};

fn stripPrefix(text: []const u8, prefix: []const u8) ?[]const u8 {
    return if (std.mem.startsWith(u8, text, prefix)) text[prefix.len..] else null;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}

test "tokenize" {
    const source =
        \\#version 330 core
        \\layout (location = 0) in vec3 aPos; // the position variable
        \\  
        \\out vec4 vertexColor; // specify a color output to the fragment shader
        \\
        \\void main()
        \\{
        \\    gl_Position = vec4(aPos, 1.0); // vec4 from vec3
        \\    vertexColor = vec4(0.5, 0.0, 0.0, 1.0); // set the output variable
        \\}
    ;

    var tokenizer = Tokenizer{ .source = source };

    const strings = [_][]const u8{
        "#version 330 core",
        "layout",
        "(",
        "location",
        "=",
        "0",
        ")",
        "in",
        "vec3",
        "aPos",
        ";",
        "// the position variable",
        "out",
        "vec4",
        "vertexColor",
        ";",
        "// specify a color output to the fragment shader",
        "void",
        "main",
        "(",
        ")",
        "{",
        "gl_Position",
        "=",
        "vec4",
        "(",
        "aPos",
        ",",
        "1.0",
        ")",
        ";",
        "// vec4 from vec3",
        "vertexColor",
        "=",
        "vec4",
        "(",
        "0.5",
        ",",
        "0.0",
        ",",
        "0.0",
        ",",
        "1.0",
        ")",
        ";",
        "// set the output variable",
        "}",
        "",
    };

    for (strings) |expected| {
        try std.testing.expectEqualStrings(expected, tokenizer.nextText() orelse "");
    }
}
