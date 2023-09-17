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

    @"==": Token,
    @"!=": Token,
    @"<": Token,
    @"<=": Token,
    @">": Token,
    @">=": Token,

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
    infix: Range,
    prefix: Range,
    postfix: Range,
    call: Range,
    argument: Range,
    selection: Range,

    type_qualifier_list: Range,
    array: Range,
    array_specifier: Range,
    field_declaration: Range,
    field: Range,

    precision_declaration: Range,
    block_declaration: Range,
    qualifier_declaration: Range,
    variable_declaration: Range,
    declaration: Range,
    function_declaration: Range,
    parameter: Range,

    initializer_list: Range,
    initializer: Range,

    statement: Range,

    case_label: Range,
    default_label: Range,

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

    fn emitDiagnostic(self: *@This(), diagnostic: Diagnostic) void {
        if (self.diagnostics) |diagnostics| {
            self.deferError(diagnostics.append(diagnostic));
        }
    }

    fn emitError(self: *@This(), message: []const u8) void {
        self.emitDiagnostic(.{
            .span = self.next.getToken() orelse unreachable,
            .message = message,
        });
        self.close(self.open(), .invalid);
    }

    fn advanceWithError(self: *@This(), message: []const u8) void {
        const m = self.open();
        self.emitDiagnostic(.{
            .span = self.next.getToken() orelse unreachable,
            .message = message,
        });
        self.advance();
        self.close(m, .invalid);
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
        self.tree.nodes.len = end;
        @memcpy(self.tree.nodes.items(.tags)[start..], tags);
        @memcpy(self.tree.nodes.items(.data)[start..], data);

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
    const m = p.open();

    if (p.eat(.@";")) return;

    if (p.eat(.keyword_precision)) {
        switch (p.peek()) {
            .keyword_lowp, .keyword_mediump, .keyword_highp => p.advance(),
            else => p.emitError("expected `lowp`, `mediump`, or `highp`"),
        }
        typeSpecifier(p);
        p.expect(.@";");
        return p.close(m, .precision_declaration);
    }

    typeQualifier(p);
    if (p.at(.identifier)) typeSpecifier(p);

    if (p.eat(.@"{")) {
        while (p.atAny(struct_field_declaration_first)) {
            structFieldDeclaration(p);
        }
        p.expect(.@"}");
        _ = p.eat(.identifier);
        arraySpecifier(p, p.open());
        p.expect(.@";");
        return p.close(m, .block_declaration);
    }

    if (p.eat(.@";")) return p.close(m, .qualifier_declaration);

    if (p.at(.@",")) {
        while (p.eat(.@",")) p.expect(.identifier);
        p.expect(.@";");
        return p.close(m, .qualifier_declaration);
    }

    var first = true;
    while (!p.eof() and !p.at(.@";")) : (first = false) {
        const m_var = p.open();
        p.expect(.identifier);

        if (first and p.eat(.@"(")) {
            // function declaration
            while (!p.eof() and !p.at(.@")")) {
                if (p.atAny(parameter_first)) {
                    parameter(p);
                } else {
                    break;
                }
            }
            p.expect(.@")");
            if (p.at(.@"{")) {
                block(p);
            } else {
                p.expect(.@";");
            }

            return p.close(m, .function_declaration);
        }

        if (p.at(.@"[")) arraySpecifier(p, p.open());
        if (p.eat(.@"=")) initializer(p);
        if (!p.at(.@";")) p.expect(.@",");
        p.close(m_var, .variable_declaration);
    }
    p.expect(.@";");
    return p.close(m, .declaration);
}

fn initializer(p: *Parser) void {
    const m = p.open();
    if (p.eat(.@"{")) {
        while (!p.eof() and !p.at(.@"}")) {
            const m_init = p.open();
            initializer(p);
            if (!p.at(.@"}")) p.expect(.@",");
            p.close(m_init, .initializer);
        }
        p.expect(.@"}");
        return p.close(m, .initializer_list);
    } else {
        assignmentExpression(p);
    }
}

const parameter_first = type_qualifier_first.unionWith(type_specifier_first);

fn parameter(p: *Parser) void {
    const m = p.open();
    typeQualifier(p);
    typeSpecifier(p);
    if (p.eat(.identifier)) {
        if (p.at(.@"[")) {
            arraySpecifier(p, p.open());
        }
    }

    if (!p.at(.@")")) p.expect(.@",");
    p.close(m, .parameter);
}

fn block(p: *Parser) void {
    p.expect(.@"{");
    while (!p.eof() and !p.at(.@"}")) {
        if (p.atAny(statement_first)) {
            statement(p);
        } else {
            break;
        }
    }
    p.expect(.@"}");
}

const statement_first = TokenSet.initMany(&.{
    .@";",
    .@"{",
    .keyword_do,
    .keyword_while,
    .keyword_for,
    .keyword_if,
    .keyword_switch,
    .keyword_case,
    .keyword_default,
    .keyword_break,
    .keyword_continue,
    .keyword_return,
}).unionWith(type_qualifier_first).unionWith(type_specifier_first).unionWith(expression_first);

fn statement(p: *Parser) void {
    const m = p.open();

    switch (p.peek()) {
        .@"{" => block(p),
        .keyword_do => {
            p.advance();
            block(p);
            p.expect(.keyword_while);
            p.expect(.@"(");
            expression(p);
            p.expect(.@")");
            p.expect(.@";");
        },
        .keyword_while => {
            p.advance();
            p.expect(.@"(");
            expression(p);
            p.expect(.@")");
            block(p);
        },
        .keyword_for => {
            p.advance();
            p.expect(.@"(");
            statement(p);
            statement(p);
            _ = expressionOpt(p);
            expression(p);
            p.expect(.@")");
            block(p);
        },
        .keyword_if => {
            p.advance();
            p.expect(.@"(");
            expression(p);
            p.expect(.@")");
            statement(p);
            if (p.eat(.keyword_else)) statement(p);
        },
        .keyword_switch => {
            p.advance();
            p.expect(.@"(");
            expression(p);
            p.expect(.@")");
            block(p);
        },
        .keyword_case => {
            expression(p);
            return p.close(m, .case_label);
        },
        .keyword_default => {
            p.expect(.@":");
            return p.close(m, .default_label);
        },
        .keyword_break, .keyword_continue, .keyword_discard => {
            p.advance();
            p.expect(.@";");
        },
        .keyword_return => {
            p.advance();
            expression(p);
            p.expect(.@";");
        },
        else => {
            if (!expressionOpt(p)) p.emitError("expected a statement");
            p.expect(.@";");
        },
    }

    p.close(m, .statement);
}

const expression_first = primary_expression_first.unionWith(unary_operators);

pub fn expression(p: *Parser) void {
    const m = p.open();
    assignmentExpression(p);
    while (p.eat(.@",")) {
        assignmentExpression(p);
        p.close(m, .expression_sequence);
    }
}

fn expressionOpt(p: *Parser) bool {
    const m = p.open();
    if (!assignmentExpressionOpt(p)) return false;
    while (p.eat(.@",")) {
        assignmentExpression(p);
        p.close(m, .expression_sequence);
    }
    return true;
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
    if (!assignmentExpressionOpt(p)) p.emitError("expected an expression");
}

fn assignmentExpressionOpt(p: *Parser) bool {
    const m = p.open();
    if (!constantExpressionOpt(p)) return false;
    while (p.atAny(assignment_operator)) {
        p.advance();
        assignmentExpression(p);
        p.close(m, .assignment);
    }
    return true;
}

fn constantExpression(p: *Parser) void {
    if (!constantExpressionOpt(p)) p.emitError("expected an expression");
}

fn constantExpressionOpt(p: *Parser) bool {
    const m = p.open();
    if (!infixExpressionOpt(p)) return false;
    if (p.eat(.@"?")) {
        expression(p);
        p.expect(.@":");
        assignmentExpression(p);
        p.close(m, .conditional);
    }
    return true;
}

const infix_operator_precedence = .{
    .{ .@"*", .@"/", .@"%" },
    .{ .@"+", .@"-" },
    .{ .@"<<", .@">>" },
    .{ .@"<", .@">", .@"<=", .@">=" },
    .{ .@"==", .@"!=" },
    .{.@"&"},
    .{.@"^"},
    .{.@"|"},
    .{.@"&&"},
    .{.@"^^"},
    .{.@"||"},
};

const precedence_level_map = blk: {
    var map = std.EnumMap(Node.Tag, u8){};
    for (infix_operator_precedence, 0..) |level, index| {
        for (level) |op| {
            map.put(op, index);
        }
    }
    break :blk map;
};

fn leftBindsStronger(lhs: Node.Tag, rhs: Node.Tag) bool {
    const lhs_level = precedence_level_map.get(lhs) orelse return true;
    const rhs_level = precedence_level_map.get(rhs) orelse return false;
    return lhs_level < rhs_level;
}

fn infixExpressionOpt(p: *Parser) bool {
    return infixExpressionOptImpl(p, .eof);
}

fn infixExpressionOptImpl(p: *Parser, lhs: Node.Tag) bool {
    const m = p.open();
    if (!unaryExpressionOpt(p)) return false;

    while (true) {
        const rhs = p.peek();
        if (leftBindsStronger(lhs, rhs)) break;
        p.advance();
        if (!infixExpressionOptImpl(p, rhs)) {
            p.emitError("expected an expression");
        }
        p.close(m, .infix);
    }

    return true;
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
    if (has_operator) p.close(m, .prefix);

    return has_operator or has_expression;
}

const postfix_operators = TokenSet.initMany(&.{
    .@"[",
    .@"(",
    .@".",
    .@"++",
    .@"--",
});

const expression_recovery = TokenSet.initMany(&.{ .@";", .@"{", .@"}" });

fn postfixExpressionOpt(p: *Parser) bool {
    const m = p.open();
    if (!primaryExpressionOpt(p)) return false;

    while (true) {
        switch (p.peek()) {
            .@"[" => arraySpecifier(p, m),
            .@"(" => {
                p.advance();
                while (!p.eof() and !p.at(.@")")) {
                    if (p.atAny(expression_first)) {
                        const m_arg = p.open();
                        assignmentExpression(p);
                        if (!p.at(.@")")) p.expect(.@",");
                        p.close(m_arg, .argument);
                    } else if (p.atAny(expression_recovery)) {
                        break;
                    } else {
                        p.advanceWithError("expected an argument");
                    }
                }
                p.expect(.@")");
                p.close(m, .call);
            },
            .@"++", .@"--" => {
                p.advance();
                p.close(m, .postfix);
            },
            .@"." => {
                p.advance();
                p.expect(.identifier);
                p.close(m, .selection);
            },
            else => break,
        }
    }

    return true;
}

const primary_expression_first = TokenSet.initMany(&.{
    .identifier,
    .number,
    .keyword_true,
    .keyword_false,
    .@"(",
    .keyword_struct,
});

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
        .keyword_struct => {
            p.advance();
            _ = p.eat(.identifier);
            p.expect(.@"{");
            while (p.atAny(struct_field_declaration_first)) {
                structFieldDeclaration(p);
            }
            p.expect(.@"}");
            return true;
        },
        else => return false,
    }
}

const struct_field_declaration_first = TokenSet.initMany(&.{})
    .unionWith(type_qualifier_first)
    .unionWith(type_specifier_first);

fn structFieldDeclaration(p: *Parser) void {
    const m = p.open();
    typeQualifier(p);
    typeSpecifier(p);
    while (true) {
        const m_field = p.open();
        if (!p.eat(.identifier)) break;
        if (p.at(.@"[")) arraySpecifier(p, p.open());
        if (!p.at(.@";")) p.expect(.@",");
        p.close(m_field, .field);
    }
    p.expect(.@";");
    p.close(m, .field_declaration);
}

const type_specifier_first = TokenSet.initMany(&.{.identifier});

fn typeSpecifier(p: *Parser) void {
    const m = p.open();
    p.expect(.identifier);
    if (p.at(.@"[")) arraySpecifier(p, m);
}

fn arraySpecifier(p: *Parser, m: Parser.Mark) void {
    while (true) {
        const inner = p.open();
        if (!p.eat(.@"[")) break;
        _ = constantExpressionOpt(p);
        p.expect(.@"]");
        p.close(inner, .array);
    }
    p.close(m, .array_specifier);
}

fn typeQualifier(p: *Parser) void {
    var any = false;
    const m = p.open();
    while (p.atAny(type_qualifier_first)) {
        any = true;
        typeQualifierSingle(p);
    }
    if (any) p.close(m, .type_qualifier_list);
}

const type_qualifier_first = TokenSet.initMany(&.{
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
    .keyword_layout,
    .keyword_subroutine,
    .keyword_highp,
    .keyword_mediump,
    .keyword_lowp,
    .keyword_smooth,
    .keyword_flat,
    .keyword_noperspective,
    .keyword_invariant,
    .keyword_precise,
});

fn typeQualifierSingle(p: *Parser) void {
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
                switch (p.peek()) {
                    .identifier => {
                        p.advance();
                        if (p.eat(.@"=")) constantExpression(p);
                    },
                    .keyword_shared => p.advance(),
                    else => break,
                }
                if (p.at(.@")")) break;
                p.expect(.@",");
            }
            p.expect(.@")");
        },

        // precision_qualifier
        .keyword_highp, .keyword_mediump, .keyword_lowp => p.advance(),
        // interpolation_qualifier
        .keyword_smooth, .keyword_flat, .keyword_noperspective => p.advance(),
        // invariant_qualifier
        .keyword_invariant => p.advance(),
        // precise_qualifier
        .keyword_precise => p.advance(),

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
                    @setEvalBranchQuota(2000);
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
