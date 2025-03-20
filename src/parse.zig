const std = @import("std");
const lsp = @import("lsp.zig");
const util = @import("util.zig");

pub const ParseOptions = struct {
    /// Append any ignored tokens (comments or preprocessor directives) to this list.
    ignored: ?*std.ArrayList(Span) = null,
    diagnostics: ?*std.ArrayList(Diagnostic) = null,
};

pub fn parse(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: ParseOptions,
) !Tree {
    var parser = Parser.init(allocator, source, options);
    defer parser.deinit();

    parser.advance();
    parseFile(&parser);
    return parser.finish();
}

pub const Tag = enum(u8) {
    eof = 0,

    unknown,

    comment,

    // a preprocessor directive: includes everyting from the leading `#` until
    // the next line ending. May contain comments and line continuations (`\\\n`).
    preprocessor,

    identifier,
    number,
    string,

    keyword_const,
    keyword_uniform,
    keyword_buffer,
    keyword_shared,
    keyword_attribute,
    keyword_varying,

    keyword_coherent,
    keyword_volatile,
    keyword_restrict,
    keyword_readonly,
    keyword_writeonly,

    keyword_layout,
    keyword_centroid,
    keyword_flat,
    keyword_smooth,
    keyword_noperspective,

    keyword_patch,
    keyword_sample,

    keyword_invariant,
    keyword_precise,

    keyword_break,
    keyword_continue,
    keyword_do,
    keyword_for,
    keyword_while,
    keyword_switch,
    keyword_case,
    keyword_default,
    keyword_if,
    keyword_else,

    keyword_subroutine,

    keyword_in,
    keyword_out,
    keyword_inout,

    keyword_discard,
    keyword_return,

    keyword_true,
    keyword_false,

    keyword_lowp,
    keyword_mediump,
    keyword_highp,
    keyword_precision,

    keyword_struct,

    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",

    @".",
    @",",
    @":",
    @";",

    @"?",

    @"~",
    @"!",

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

    @"==",
    @"!=",
    @"<",
    @"<=",
    @">",
    @">=",

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

    @"&&",
    @"||",
    @"^^",

    @"++",
    @"--",

    /// KEEP THIS RIGHT AFTER THE TOKENS.
    /// An invalid syntax tree.
    invalid,

    parenthized,
    expression_sequence,
    assignment,
    conditional,
    infix,
    prefix,
    postfix,
    call,
    argument,
    selection,

    type_qualifier_list,
    array,
    array_specifier,
    struct_specifier,

    field_declaration_list,

    subroutine_qualifier,
    layout_qualifier,
    precision_declaration,
    block_declaration,
    qualifier_declaration,
    variable_declaration,
    variable_declaration_list,
    declaration,
    function_declaration,
    parameter_list,
    parameter,

    initializer_list,
    initializer,

    block,
    statement,
    condition_list,
    if_branch,
    else_branch,

    case_label,
    default_label,

    file,

    pub fn isToken(tag: @This()) bool {
        return @intFromEnum(tag) < @intFromEnum(Tag.invalid);
    }

    pub fn isSyntax(tag: @This()) bool {
        return !tag.isToken();
    }
};

pub const Node = struct {
    /// The type of node.
    tag: Tag,
    /// If this is a token, the byte offset in the source file. Otherwise, the
    /// indices of the child nodes.
    span: Span,
    /// The index of the parent node.
    parent: u32 = undefined,

    pub fn getToken(self: Node) ?Span {
        return if (self.tag.isToken()) self.span else null;
    }

    pub fn getRange(self: Node) ?Range {
        return if (self.tag.isSyntax()) self.span else null;
    }
};

pub const Tree = struct {
    nodes: std.MultiArrayList(Node) = .{},
    /// Index of the root node. Any nodes after this are either comments or preprocessors.
    root: u32,

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.nodes.deinit(allocator);
    }

    fn assignParents(self: *@This()) void {
        const nodes = &self.nodes;
        for (nodes.items(.tag), nodes.items(.span), 0..) |tag_, span, index| {
            if (!tag_.isSyntax()) continue;
            @memset(nodes.items(.parent)[span.start..span.end], @intCast(index));
        }
    }

    pub fn tag(self: @This(), index: usize) Tag {
        return self.nodes.items(.tag)[index];
    }

    pub fn parent(self: @This(), index: usize) ?u32 {
        if (self.tag(index) == .file) return null;
        return self.nodes.items(.parent)[index];
    }

    pub fn token(self: @This(), index: usize) Token {
        return self.nodes.items(.span)[index];
    }

    pub fn children(self: @This(), index: usize) Range {
        return self.nodes.items(.span)[index];
    }

    pub fn rootIndex(self: @This()) u32 {
        return self.root;
    }

    pub fn ignoredStart(self: @This()) u32 {
        return self.root + 1;
    }

    pub fn ignored(self: @This()) []Token {
        return self.nodes.items(.span)[self.root + 1 ..];
    }

    pub fn nodeSpan(self: @This(), node: u32) Span {
        return .{
            .start = self.nodeSpanExtreme(node, .start),
            .end = self.nodeSpanExtreme(node, .end),
        };
    }

    pub fn nodeSpanExtreme(
        self: @This(),
        node: u32,
        comptime extreme: std.meta.FieldEnum(Span),
    ) u32 {
        const tags = self.nodes.items(.tag);
        const spans = self.nodes.items(.span);
        var current = node;
        while (true) {
            const span = spans[current];
            const value = @field(span, @tagName(extreme));
            if (tags[current].isToken()) return value;
            if (span.start == span.end) return 0;
            current = if (extreme == .start) value else value - 1;
        }
    }

    pub fn format(tree: @This(), source: []const u8) std.fmt.Formatter(formatWithSource) {
        return .{ .data = .{
            .tree = tree,
            .source = source,
        } };
    }

    const WithSource = struct {
        tree: Tree,
        source: []const u8,
    };

    fn formatWithSource(
        data: WithSource,
        comptime fmt: []const u8,
        _: anytype,
        writer: anytype,
    ) !void {
        comptime var with_spans = false;

        if (comptime std.mem.eql(u8, fmt, "..")) {
            with_spans = true;
        } else if (fmt.len != 0) {
            @compileError("expected `{}` or `{..}`");
        }

        const Formatter = struct {
            tree: Tree,
            writer: @TypeOf(writer),
            source: []const u8,
            indent: usize = 0,

            fn writeNode(self: *@This(), index: usize) !void {
                const indent = self.indent;

                const node = self.tree.nodes.get(index);
                const name = @tagName(node.tag);

                try self.writer.writeByteNTimes(' ', indent);

                if (node.getToken()) |tok| {
                    const text = self.source[tok.start..tok.end];
                    if (std.ascii.isAlphabetic(name[0])) {
                        try self.writer.writeAll(name);
                        try self.writer.print(" '{'}'", .{std.zig.fmtEscapes(text)});
                    } else {
                        try self.writer.writeAll(name);
                    }
                    if (with_spans) try self.writer.print(" {}..{}", .{ tok.start, tok.end });
                    try self.writer.writeByte('\n');
                }

                if (node.getRange()) |range| {
                    defer self.indent = indent;
                    try self.writer.writeAll(name);
                    if (with_spans) {
                        const span = self.tree.nodeSpan(@intCast(index));
                        try self.writer.print(" {}..{}", .{ span.start, span.end });
                    }
                    try self.writer.writeByte('\n');
                    self.indent += 2;
                    for (range.start..range.end) |child| {
                        try self.writeNode(child);
                    }
                }
            }
        };

        var f = Formatter{ .tree = data.tree, .source = data.source, .writer = writer };
        try f.writeNode(data.tree.root);
    }
};

const TokenSet = std.EnumSet(Tag);

pub const Span = struct {
    start: u32,
    end: u32,
};
pub const Token = Span;
pub const Range = Span;

pub const Diagnostic = struct {
    span: Span,
    message: []const u8,

    pub fn position(self: @This(), source: []const u8) lsp.Position {
        return util.positionFromUtf8(source, self.span.start);
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokenizer: Tokenizer,
    next: Node,
    last_end: u32 = 0,
    fuel: u32 = max_fuel,

    deferred_error: ?Error = null,

    tree: Tree = .{ .root = undefined },
    stack: std.MultiArrayList(Node) = .{},

    options: ParseOptions,

    const max_fuel = 256;

    pub const Error = error{OutOfMemory};

    pub fn init(allocator: std.mem.Allocator, source: []const u8, options: ParseOptions) @This() {
        return .{
            .allocator = allocator,
            .tokenizer = Tokenizer{ .source = source },
            .next = .{ .tag = .eof, .span = .{ .start = 0, .end = 0 } },
            .options = options,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.stack.deinit(self.allocator);
        self.tree.deinit(self.allocator);
    }

    pub fn finish(self: *@This()) !Tree {
        if (self.deferred_error) |err| return err;

        try self.tree.nodes.append(self.allocator, self.stack.pop());
        self.tree.assignParents();

        var tree = self.tree;
        self.tree = .{ .root = undefined };

        tree.root = @intCast(tree.nodes.len - 1);

        // Append any ignored tokens to the end of the parse tree.
        // This ensures that we can refer to ignored tokens with the same
        // mechanisms as any other syntax node.
        if (self.options.ignored) |ignored| {
            const ignored_start = tree.nodes.len;
            try tree.nodes.resize(self.allocator, tree.nodes.len + ignored.items.len);
            for (
                ignored.items,
                tree.nodes.items(.tag)[ignored_start..],
                tree.nodes.items(.span)[ignored_start..],
                tree.nodes.items(.parent)[ignored_start..],
            ) |token, *tag, *span, *parent| {
                tag.* = if (self.tokenizer.source[token.start] == '#') .preprocessor else .comment;
                span.* = token;
                parent.* = tree.root;
            }
        }

        return tree;
    }

    fn deferError(self: *@This(), value: anytype) @typeInfo(@TypeOf(value)).error_union.payload {
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
            switch (token.tag) {
                .comment, .preprocessor => {
                    if (self.options.ignored) |ignored| {
                        self.deferError(ignored.append(token.getToken().?));
                    }
                },
                else => {
                    self.last_end = self.next.span.end;
                    self.next = token;
                    self.fuel = max_fuel;
                    return;
                },
            }
        }
    }

    fn peek(self: *@This()) Tag {
        self.fuel -= 1;
        if (self.fuel == 0) std.debug.panic("parser ran out of fuel (infinite loop?): {}", .{self.next.tag});
        return self.next.tag;
    }

    fn at(self: *@This(), expected: Tag) bool {
        return self.peek() == expected;
    }

    fn atAny(self: *@This(), set: TokenSet) bool {
        return set.contains(self.peek());
    }

    fn eof(self: *@This()) bool {
        return self.at(.eof);
    }

    fn eat(self: *@This(), expected: Tag) bool {
        const found = self.at(expected);
        if (found) self.advance();
        return found;
    }

    fn expect(self: *@This(), comptime expected: Tag) void {
        if (!self.eat(expected)) {
            self.emitError("expected " ++ @tagName(expected));
        }
    }

    fn emitDiagnostic(self: *@This(), diagnostic: Diagnostic) void {
        if (self.options.diagnostics) |diagnostics| {
            self.deferError(diagnostics.append(diagnostic));
        }
    }

    fn emitError(self: *@This(), message: []const u8) void {
        self.emitDiagnostic(.{
            .span = .{
                .start = self.last_end,
                .end = self.last_end,
            },
            .message = message,
        });

        const m = self.open();
        self.close(m, .invalid);
    }

    fn advanceWithError(self: *@This(), message: []const u8) void {
        const m = self.open();
        self.emitDiagnostic(.{
            .span = self.next.span,
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

    fn closeRange(self: *@This(), mark: Mark) Range {
        const tags = self.stack.items(.tag)[mark.index..];
        const spans = self.stack.items(.span)[mark.index..];

        const start = self.tree.nodes.len;
        const end = start + tags.len;

        self.deferError(self.tree.nodes.ensureUnusedCapacity(self.allocator, tags.len));
        self.tree.nodes.len = end;
        @memcpy(self.tree.nodes.items(.tag)[start..], tags);
        @memcpy(self.tree.nodes.items(.span)[start..], spans);

        self.stack.len = mark.index;

        return Range{
            .start = @intCast(start),
            .end = @intCast(end),
        };
    }

    fn close(self: *@This(), mark: Mark, comptime tag: Tag) void {
        comptime std.debug.assert(tag.isSyntax());

        const range = self.closeRange(mark);

        if (tag == .invalid) {
            // merge the nodes if they are adjacent

            const tags = self.stack.items(.tag);
            if (tags.len != 0 and tags[tags.len - 1] == .invalid) {
                const spans = self.stack.items(.span);
                if (spans[spans.len - 1].end == range.start) {
                    spans[spans.len - 1].end = range.end;
                    return;
                }
            }
        }

        self.deferError(self.stack.append(self.allocator, .{
            .tag = tag,
            .span = range,
        }));
    }

    fn lastTag(self: *const @This()) Tag {
        const tags = self.stack.items(.tag);
        return tags[tags.len - 1];
    }
};

pub fn parseFile(p: *Parser) void {
    const m = p.open();

    while (!p.eof()) {
        if (p.atAny(external_declaration_first)) {
            externalDeclaration(p);
        } else {
            p.advanceWithError("expected a declaration");
        }
    }

    p.close(m, .file);
}

const external_declaration_first = TokenSet.initMany(&.{.@";"})
    .unionWith(type_qualifier_first)
    .unionWith(type_specifier_first);

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

    const identifier_specifier = p.at(.identifier);
    if (p.atAny(type_specifier_first)) typeSpecifier(p);

    if (identifier_specifier and p.lastTag() == .identifier and p.at(.@"(")) {
        // looks like macro expansion
        var level: u32 = 0;
        while (true) {
            const tag = p.peek();
            defer p.advance();
            switch (tag) {
                .@"(" => level += 1,
                .@")" => {
                    level -= 1;
                    if (level == 0) break;
                },
                .eof => break,
                else => {},
            }
        }
        return p.close(m, .call);
    }

    const m_field_list = p.open();
    if (p.eat(.@"{")) {
        while (p.atAny(struct_field_declaration_first)) {
            structFieldDeclaration(p);
        }
        p.expect(.@"}");
        p.close(m_field_list, .field_declaration_list);
        if (p.at(.identifier)) variableDeclaration(p);
        p.expect(.@";");
        return p.close(m, .block_declaration);
    }

    if (identifier_specifier and p.at(.@",")) {
        while (p.eat(.@",")) p.expect(.identifier);
        p.expect(.@";");
        return p.close(m, .qualifier_declaration);
    }

    const m_vars = p.open();

    var count: u32 = 0;
    while (!p.eof() and !p.at(.@";")) : (count += 1) {
        const m_var = p.open();
        if (!p.eat(.identifier)) break;

        if (count == 0) {
            const m_params = p.open();
            if (p.eat(.@"(")) {
                // function declaration
                while (!p.eof() and !p.at(.@")")) {
                    if (p.atAny(parameter_first)) {
                        parameter(p);
                    } else {
                        break;
                    }
                }
                p.expect(.@")");
                p.close(m_params, .parameter_list);

                if (p.at(.@"{")) {
                    block(p);
                } else {
                    p.expect(.@";");
                }

                return p.close(m, .function_declaration);
            }
        }

        variableDeclarationSuffix(p, m_var);
    }

    if (count > 1) p.close(m_vars, .variable_declaration_list);

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
    const m_name = p.open();
    if (p.eat(.identifier)) {
        if (p.at(.@"[")) arraySpecifier(p, m_name);
        p.close(m_name, .variable_declaration);
    }
    if (!p.at(.@")")) p.expect(.@",");

    p.close(m, .parameter);
}

fn block(p: *Parser) void {
    const m = p.open();
    p.expect(.@"{");
    while (!p.eof() and !p.at(.@"}")) {
        if (p.atAny(statement_first)) {
            statement(p);
        } else {
            break;
        }
    }
    p.expect(.@"}");
    p.close(m, .block);
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
    .keyword_discard,
    .keyword_return,
}).unionWith(type_qualifier_first)
    .unionWith(type_specifier_first)
    .unionWith(expression_first);

fn statement(p: *Parser) void {
    const m = p.open();

    switch (p.peek()) {
        .@";" => return p.advance(),
        .@"{" => return block(p),
        .keyword_do => {
            p.advance();
            block(p);
            p.expect(.keyword_while);
            {
                const m_cond = p.open();
                p.expect(.@"(");
                expression(p);
                p.expect(.@")");
                p.close(m_cond, .condition_list);
            }
            p.expect(.@";");
        },
        .keyword_while => {
            p.advance();
            {
                const m_cond = p.open();
                p.expect(.@"(");
                condition(p);
                p.expect(.@")");
                p.close(m_cond, .condition_list);
            }
            statement(p);
        },
        .keyword_for => {
            p.advance();
            {
                const m_cond = p.open();
                p.expect(.@"(");
                if (!p.eat(.@";")) conditionStatement(p);
                if (!p.eat(.@";")) conditionStatement(p);
                _ = expressionOpt(p);
                p.expect(.@")");
                p.close(m_cond, .condition_list);
            }
            statement(p);
        },
        .keyword_if => {
            var m_branch = p.open();
            while (true) {
                p.expect(.keyword_if);
                {
                    const m_cond = p.open();
                    p.expect(.@"(");
                    condition(p);
                    p.expect(.@")");
                    p.close(m_cond, .condition_list);
                }
                statement(p);
                p.close(m_branch, .if_branch);

                m_branch = p.open();

                if (!p.eat(.keyword_else)) break;
                if (p.at(.keyword_if)) continue;

                statement(p);
                p.close(m_branch, .else_branch);

                break;
            }
        },
        .keyword_switch => {
            p.advance();
            {
                const m_cond = p.open();
                p.expect(.@"(");
                condition(p);
                p.expect(.@")");
                p.close(m_cond, .condition_list);
            }
            block(p);
        },
        .keyword_case => {
            p.advance();
            expression(p);
            p.expect(.@":");
            return p.close(m, .case_label);
        },
        .keyword_default => {
            p.advance();
            p.expect(.@":");
            return p.close(m, .default_label);
        },
        .keyword_break, .keyword_continue, .keyword_discard => {
            p.advance();
            p.expect(.@";");
        },
        .keyword_return => {
            p.advance();
            _ = expressionOpt(p);
            p.expect(.@";");
        },
        else => {
            const kind = simpleStatement(p);
            p.expect(.@";");
            if (kind == .declaration) return p.close(m, .declaration);
        },
    }

    p.close(m, .statement);
}

fn conditionStatement(p: *Parser) void {
    const m = p.open();
    const kind = simpleStatement(p);
    p.expect(.@";");
    if (kind == .declaration) return p.close(m, .declaration);
}

fn condition(p: *Parser) void {
    const m = p.open();
    switch (simpleStatement(p)) {
        .declaration => p.close(m, .declaration),
        .expression => {},
    }
}

fn simpleStatement(p: *Parser) enum { declaration, expression } {
    var is_decl = false;
    var has_specifier = false;

    if (p.atAny(type_qualifier_first)) {
        typeQualifier(p);
        typeSpecifier(p);
        is_decl = true;
        has_specifier = true;
    } else {
        if (!expressionOpt(p)) p.emitError("expected a statement");
        has_specifier = switch (p.lastTag()) {
            .array_specifier, .struct_specifier, .identifier => true,
            else => false,
        };
    }

    if (has_specifier and variableDeclarationList(p) > 0) is_decl = true;

    return if (is_decl) .declaration else .expression;
}

fn variableDeclarationList(p: *Parser) u32 {
    const m_vars = p.open();
    var var_count: u32 = 0;
    while (p.at(.identifier)) : (var_count += 1) {
        variableDeclaration(p);
    }
    if (var_count > 1) p.close(m_vars, .variable_declaration_list);
    return var_count;
}

fn variableDeclaration(p: *Parser) void {
    const m_var = p.open();
    p.expect(.identifier);
    variableDeclarationSuffix(p, m_var);
}

fn variableDeclarationSuffix(p: *Parser, m_var: Parser.Mark) void {
    if (p.at(.@"[")) arraySpecifier(p, m_var);
    if (p.eat(.@"=")) initializer(p);
    if (!p.at(.@";") and !p.at(.@")")) p.expect(.@",");
    p.close(m_var, .variable_declaration);
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

pub const assignment_operators = TokenSet.initMany(&.{
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
    var has_assignment = false;
    while (p.atAny(assignment_operators)) {
        p.advance();
        has_assignment = true;
        assignmentExpression(p);
    }
    if (has_assignment) p.close(m, .assignment);
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
    var map = std.EnumArray(Tag, u8).initFill(255);
    for (infix_operator_precedence, 0..) |level, index| {
        for (level) |op| {
            map.set(op, @truncate(index));
        }
    }
    break :blk map;
};

pub const infix_operators = blk: {
    var set = TokenSet.initEmpty();
    for (infix_operator_precedence) |level| for (level) |op| set.insert(op);
    break :blk set;
};

fn leftBindsStronger(lhs: Tag, rhs: Tag) bool {
    const lhs_level = precedence_level_map.get(lhs);
    const rhs_level = precedence_level_map.get(rhs);
    return lhs_level <= rhs_level;
}

fn infixExpressionOpt(p: *Parser) bool {
    return infixExpressionOptImpl(p, .eof);
}

fn infixExpressionOptImpl(p: *Parser, lhs: Tag) bool {
    const m = p.open();
    if (!unaryExpressionOpt(p)) return false;

    var has_operator = false;
    while (true) {
        const rhs = p.peek();
        if (leftBindsStronger(lhs, rhs)) break;
        p.advance();
        has_operator = true;

        if (!infixExpressionOptImpl(p, rhs)) {
            p.emitError("expected an expression");
        }
    }

    if (has_operator) p.close(m, .infix);

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
            structSpecifier(p);
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
    if (variableDeclarationList(p) == 0) p.emitError("expected an identifier");
    p.expect(.@";");
    p.close(m, .declaration);
}

const type_specifier_first = TokenSet.initMany(&.{ .identifier, .keyword_struct });

fn typeSpecifier(p: *Parser) void {
    if (p.at(.keyword_struct)) return structSpecifier(p);

    const m = p.open();
    p.expect(.identifier);
    if (p.at(.@"[")) arraySpecifier(p, m);
}

fn structSpecifier(p: *Parser) void {
    const m = p.open();
    p.advance();
    _ = p.eat(.identifier);

    const m_field_list = p.open();
    p.expect(.@"{");
    while (p.atAny(struct_field_declaration_first)) {
        structFieldDeclaration(p);
    }
    p.expect(.@"}");
    p.close(m_field_list, .field_declaration_list);
    p.close(m, .struct_specifier);
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
    .keyword_varying,
    .keyword_attribute,
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
            const m = p.open();
            p.advance();
            p.expect(.@"(");
            while (true) {
                switch (p.peek()) {
                    .identifier => {
                        const m_attr = p.open();
                        p.advance();
                        if (p.eat(.@"=")) {
                            constantExpression(p);
                            p.close(m_attr, .assignment);
                        }
                    },
                    .keyword_shared => p.advance(),
                    else => break,
                }
                if (p.at(.@")")) break;
                p.expect(.@",");
            }
            p.expect(.@")");
            p.close(m, .layout_qualifier);
        },

        // precision_qualifier
        .keyword_highp, .keyword_mediump, .keyword_lowp => p.advance(),
        // interpolation_qualifier
        .keyword_smooth, .keyword_flat, .keyword_noperspective => p.advance(),
        // invariant_qualifier
        .keyword_invariant => p.advance(),
        // precise_qualifier
        .keyword_precise => p.advance(),

        .keyword_varying, .keyword_attribute => p.advance(),

        .keyword_subroutine => {
            const m = p.open();
            p.advance();
            if (p.eat(.@"(")) {
                while (p.eat(.identifier)) {
                    if (!p.at(.@")")) p.expect(.@",");
                }
                p.expect(.@")");
            }
            p.close(m, .subroutine_qualifier);
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

                if (i < N and text[i] == '.') {
                    // fractional part
                    i += 1;
                    while (i < N and std.ascii.isDigit(text[i])) i += 1;
                }

                if (first == '.' and i == self.offset + 1) {
                    // we have only parsed the dot without any digits.
                    return self.token(.@".", i);
                }

                if (i < N and (text[i] == 'e' or text[i] == 'E')) {
                    // exponent
                    i += 1;
                    if (i < N and (text[i] == '+' or text[i] == '-')) i += 1;
                    while (i < N and std.ascii.isDigit(text[i])) i += 1;
                }

                // type suffix (we just accept anything here to be as permissive as possible)
                while (i < N and isIdentifierChar(text[i])) i += 1;

                return self.token(.number, i);
            },

            'a'...'z', 'A'...'Z', '_' => {
                i += 1;
                while (i < N and isIdentifierChar(text[i])) i += 1;
                const ident = self.tokenSpan(i);
                const tag = mapKeyword(self.source[ident.start..ident.end]);
                return .{ .tag = tag, .span = ident };
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
                '/', '*' => return self.token(.comment, stripComment(text, i)),
                '=' => return self.token(.@"/=", i + 2),
                else => return self.token(.@"/", i + 1),
            },
            '%' => switch (getOrZero(text, i + 1)) {
                '=' => return self.token(.@"%=", i + 2),
                else => return self.token(.@"%", i + 1),
            },
            '&' => switch (getOrZero(text, i + 1)) {
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
                while (i < N) {
                    switch (text[i]) {
                        '\n' => break,
                        '\\' => i = @max(i + 1, stripLineEscape(text, i)),
                        '/' => i = @max(i + 1, stripComment(text, i)),
                        else => i += 1,
                    }
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
                    for (std.meta.tags(Tag)) |tag| {
                        const name = @tagName(tag);
                        valid_char.set(name[0]);
                    }
                }

                while (i < N and !valid_char.isSet(text[i])) i += 1;

                if (i == self.offset) {
                    std.debug.panic("encountered empty token at position {}: {c}", .{ i, text[i] });
                }

                return self.token(.unknown, i);
            },
        }
    }

    fn getOrZero(text: []const u8, index: u32) u8 {
        return if (index < text.len) text[index] else 0;
    }

    fn token(self: *@This(), comptime tag: Tag, end: u32) Node {
        return .{ .tag = tag, .span = self.tokenSpan(end) };
    }

    fn tokenSpan(self: *@This(), end: u32) Span {
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

    fn mapKeyword(identifier: []const u8) Tag {
        const map = comptime blk: {
            @setEvalBranchQuota(2000);

            const tags = std.meta.tags(Tag);

            var table = std.BoundedArray(struct { []const u8, Tag }, tags.len){};

            for (tags) |tag| {
                if (stripPrefix(@tagName(tag), "keyword_")) |name| {
                    table.appendAssumeCapacity(.{ name, tag });
                }
            }

            break :blk std.StaticStringMap(Tag).initComptime(table.slice());
        };

        return map.get(identifier) orelse .identifier;
    }
};

fn stripLineEscape(text: []const u8, start: u32) u32 {
    if (std.mem.startsWith(u8, text[start..], "\\\n")) return start + 2;
    if (std.mem.startsWith(u8, text[start..], "\\\r\n")) return start + 3;
    return start;
}

fn stripComment(text: []const u8, start: u32) u32 {
    var i = start;

    if (std.mem.startsWith(u8, text[i..], "//")) {
        i += 2;
        while (i < text.len) {
            switch (text[i]) {
                '\n' => break,
                '\\' => i = @max(i + 1, stripLineEscape(text, i)),
                else => i += 1,
            }
        }
    } else if (std.mem.startsWith(u8, text[i..], "/*")) {
        i += 2;
        while (i < text.len) : (i += 1) {
            if (std.mem.startsWith(u8, text[i..], "*/")) {
                i += 2;
                break;
            }
        }
    }

    return i;
}

fn stripPrefix(text: []const u8, prefix: []const u8) ?[]const u8 {
    return if (std.mem.startsWith(u8, text, prefix)) text[prefix.len..] else null;
}

pub const Directive = union(enum) {
    define: struct { name: Span },
    include: struct { path: Span },
    version: struct { number: Span },
    extension: struct { name: Span },
};

pub fn parsePreprocessorDirective(line: []const u8) ?Directive {
    var i: u32 = 0;

    i = skipWhitespace(i, line);
    if (!skipChar(i, line, '#')) return null;
    i += 1;

    i = skipWhitespace(i, line);
    const kind_start = i;
    const kind_end = skipIdentifier(i, line);
    const kind = line[kind_start..kind_end];
    i = kind_end;

    i = skipWhitespace(i, line);

    if (std.mem.eql(u8, kind, "define")) {
        const name_start = i;
        const name_end = skipIdentifier(i, line);
        if (name_start == name_end) return null;
        return .{ .define = .{ .name = .{ .start = name_start, .end = name_end } } };
    }

    if (std.mem.eql(u8, kind, "include")) {
        const terminator: u8 = if (skipChar(i, line, '"'))
            '"'
        else if (skipChar(i, line, '<'))
            '>'
        else
            return null;
        i += 1;

        const path_start = i;
        while (i < line.len and line[i] != terminator) i += 1;
        const path_end = i;

        return .{ .include = .{ .path = .{ .start = path_start, .end = path_end } } };
    }

    if (std.mem.eql(u8, kind, "extension")) {
        const name_start = i;
        const name_end = skipIdentifier(i, line);
        if (name_start == name_end) return null;
        return .{ .extension = .{ .name = .{ .start = name_start, .end = name_end } } };
    }

    if (std.mem.eql(u8, kind, "version")) {
        const number_start = i;
        const number_end = skipIdentifier(i, line);
        if (number_start == number_end) return null;
        return .{ .version = .{ .number = .{ .start = number_start, .end = number_end } } };
    }

    return null;
}

fn skipChar(index: u32, text: []const u8, char: u8) bool {
    return index < text.len and text[index] == char;
}

fn skipWhitespace(start: u32, text: []const u8) u32 {
    var i = start;
    while (i < text.len and std.ascii.isWhitespace(text[i])) i += 1;
    return i;
}

fn skipIdentifier(start: u32, text: []const u8) u32 {
    var i = start;
    while (i < text.len and Tokenizer.isIdentifierChar(text[i])) i += 1;
    return i;
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

test "parse and write" {
    const source =
        \\#version 330 core
        \\layout (location = 0) in vec3 aPos; // the position variable
    ;

    var tree = try parse(std.testing.allocator, source, .{});
    defer tree.deinit(std.testing.allocator);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try buffer.writer().print("{}", .{tree.format(source)});

    try std.testing.expectEqualStrings(
        \\file
        \\  declaration
        \\    type_qualifier_list
        \\      layout_qualifier
        \\        keyword_layout 'layout'
        \\        (
        \\        assignment
        \\          identifier 'location'
        \\          =
        \\          number '0'
        \\        )
        \\      keyword_in 'in'
        \\    identifier 'vec3'
        \\    variable_declaration
        \\      identifier 'aPos'
        \\    ;
        \\
    , buffer.items);
}

test "parse infix op" {
    const source =
        \\ int x = 1 + 2;
    ;

    var tree = try parse(std.testing.allocator, source, .{});
    defer tree.deinit(std.testing.allocator);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try buffer.writer().print("{}", .{tree.format(source)});

    try std.testing.expectEqualStrings(
        \\file
        \\  declaration
        \\    identifier 'int'
        \\    variable_declaration
        \\      identifier 'x'
        \\      =
        \\      infix
        \\        number '1'
        \\        +
        \\        number '2'
        \\    ;
        \\
    , buffer.items);
}

test "parse logical operator" {
    const source =
        \\bool x = true && true;
    ;

    var tree = try parse(std.testing.allocator, source, .{});
    defer tree.deinit(std.testing.allocator);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try buffer.writer().print("{}", .{tree.format(source)});

    try std.testing.expectEqualStrings(
        \\file
        \\  declaration
        \\    identifier 'bool'
        \\    variable_declaration
        \\      identifier 'x'
        \\      =
        \\      infix
        \\        keyword_true 'true'
        \\        &&
        \\        keyword_true 'true'
        \\    ;
        \\
    , buffer.items);
}

test "parse switch" {
    // From: https://github.com/nolanderc/glsl_analyzer/issues/7
    try expectParsesOkay(
        \\#version 430 core
        \\
        \\void main() {
        \\    switch (i) {
        \\         case 1: break;
        \\    }
        \\}
    );
}

test "parse discard" {
    // From: https://github.com/nolanderc/glsl_analyzer/issues/15
    try expectParsesOkay(
        \\#version 330 core
        \\void main() {
        \\    if (true) discard;
        \\}
    );
    try expectParsesOkay(
        \\#version 330 core
        \\void main() {
        \\    if (true) { discard; }
        \\}
    );
}

test "parse field selector" {
    // From: https://github.com/nolanderc/glsl_analyzer/issues/15
    try expectParsesOkay(
        \\#version 330 core
        \\void main() {
        \\    foo.flag;
        \\}
    );
}

fn expectParsesOkay(source: []const u8) !void {
    var diagnostics = std.ArrayList(Diagnostic).init(std.testing.allocator);
    defer diagnostics.deinit();

    var tree = try parse(std.testing.allocator, source, .{ .diagnostics = &diagnostics });
    defer tree.deinit(std.testing.allocator);

    errdefer std.debug.print("======== source ========\n{s}\n========================\n", .{source});
    errdefer std.log.err("tree:\n{}", .{tree.format(source)});

    if (diagnostics.items.len != 0) {
        for (diagnostics.items) |diagnostic| {
            const position = diagnostic.position(source);
            std.log.err("{}:{}: {s}", .{ position.line + 1, position.character + 1, diagnostic.message });
        }
        return error.FoundDiagnostics;
    }

    for (tree.nodes.items(.tag)) |tag| {
        if (tag == .invalid) {
            return error.FoundInvalidSyntaxNode;
        }
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
