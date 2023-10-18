const std = @import("std");
const parse = @import("parse.zig");
const util = @import("util.zig");
const Tree = parse.Tree;
const Node = parse.Node;
const Workspace = @import("Workspace.zig");
const Document = @import("Document.zig");
const syntax = @import("syntax.zig");

pub const Reference = struct {
    /// The document in which the reference was found.
    document: *Document,
    /// Index of the syntax node where the reference is declared.
    node: u32,
    /// Index of the parent node which declares the identifier.
    parent_declaration: u32,

    pub fn span(self: @This()) parse.Span {
        const parsed = &self.document.parse_tree.?;
        return parsed.tree.nodeSpan(self.node);
    }

    pub fn name(self: @This()) []const u8 {
        const parsed = &self.document.parse_tree.?;
        if (nodeName(parsed.tree, self.node, self.document.source())) |n| return n;
        const s = self.span();
        return self.document.source()[s.start..s.end];
    }
};

pub const Type = struct {
    qualifiers: ?syntax.QualifierList = null,
    specifier: ?syntax.TypeSpecifier = null,
    block_fields: ?syntax.FieldList = null,
    arrays: ?syntax.ListIterator(syntax.Array) = null,
    parameters: ?syntax.ParameterList = null,

    pub fn format(self: @This(), tree: Tree, source: []const u8) std.fmt.Formatter(formatType) {
        return .{ .data = .{ .tree = tree, .source = source, .type = self } };
    }

    fn prettifyOptions(node: u32) @import("format.zig").FormatOptions {
        return .{ .root_node = node, .single_line = true };
    }

    fn formatType(
        data: struct { tree: Tree, source: []const u8, type: Type },
        _: anytype,
        _: anytype,
        writer: anytype,
    ) !void {
        const prettify = @import("format.zig").format;

        var first = true;

        if (data.type.qualifiers) |qualifiers| {
            try prettify(data.tree, data.source, writer, prettifyOptions(qualifiers.node));
            first = false;
        }
        if (data.type.specifier) |specifier| {
            if (!first) try writer.writeByte(' ');
            try prettify(data.tree, data.source, writer, prettifyOptions(specifier.getNode()));
            first = false;
        }
        if (data.type.block_fields) |block_fields| {
            if (!first) try writer.writeByte(' ');
            try prettify(data.tree, data.source, writer, prettifyOptions(block_fields.node));
            first = false;
        }
        if (data.type.arrays) |arrays| {
            var iterator = arrays;
            while (iterator.next(data.tree)) |array| {
                try prettify(data.tree, data.source, writer, prettifyOptions(array.node));
            }
        }
        if (data.type.parameters) |parameters| {
            try writer.writeAll(" (");
            var i: u32 = 0;
            var iterator = parameters.iterator();
            while (iterator.next(data.tree)) |parameter| : (i += 1) {
                const parameter_type = parameterType(parameter, data.tree);
                if (i != 0) try writer.writeAll(", ");
                try writer.print("{}", .{parameter_type.format(data.tree, data.source)});
            }
            try writer.writeAll(")");
        }
    }
};

pub fn typeOf(reference: Reference) !?Type {
    const parsed = try reference.document.parseTree();
    const tree = parsed.tree;

    const decl = syntax.AnyDeclaration.tryExtract(tree, reference.parent_declaration) orelse return null;

    switch (decl) {
        .function => |function| {
            return .{
                .qualifiers = function.get(.qualifiers, tree),
                .specifier = function.get(.specifier, tree),
                .parameters = function.get(.parameters, tree),
            };
        },
        .struct_specifier => |spec| {
            return .{
                .specifier = .{ .struct_specifier = spec },
            };
        },
        inline else => |syntax_node| {
            return .{
                .qualifiers = syntax_node.get(.qualifiers, tree),
                .specifier = syntax_node.get(.specifier, tree),
                .block_fields = if (@TypeOf(syntax_node) == syntax.BlockDeclaration)
                    syntax_node.get(.fields, tree)
                else
                    null,
                .arrays = blk: {
                    const parent = tree.parent(reference.node) orelse break :blk null;
                    const name = syntax.VariableName.tryExtract(tree, parent) orelse break :blk null;
                    break :blk name.arrayIterator();
                },
            };
        },
    }
}

pub fn parameterType(parameter: syntax.Parameter, tree: Tree) Type {
    return .{
        .qualifiers = parameter.get(.qualifiers, tree),
        .specifier = parameter.get(.specifier, tree),
        .arrays = blk: {
            const variable = parameter.get(.variable, tree) orelse break :blk null;
            const name = variable.get(.name, tree) orelse break :blk null;
            break :blk name.arrayIterator();
        },
    };
}

test "typeOf function" {
    try expectTypeFormat(
        "void /*0*/main() {}",
        &.{"void ()"},
    );
    try expectTypeFormat(
        "int /*0*/add(int x, int y) {}",
        &.{"int (int, int)"},
    );
}

fn expectTypeFormat(source: []const u8, types: []const []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var workspace = try Workspace.init(allocator);
    defer workspace.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "file://test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    const parsed = try document.parseTree();
    const tree = parsed.tree;

    if (cursors.count() != types.len) return error.InvalidCursorCount;

    for (types, cursors.values()) |expected, cursor| {
        var references = std.ArrayList(Reference).init(allocator);
        defer references.deinit();

        try findDefinition(allocator, document, cursor.node, &references);
        if (references.items.len != 1) return error.InvalidReference;
        const ref = references.items[0];

        const typ = try typeOf(ref) orelse return error.InvalidType;

        const found = try std.fmt.allocPrint(allocator, "{}", .{typ.format(tree, document.source())});
        defer allocator.free(found);

        try std.testing.expectEqualStrings(expected, found);
    }
}

// Given a node in the given parse tree, attempts to find the node(s) it references.
pub fn findDefinition(
    arena: std.mem.Allocator,
    document: *Document,
    node: u32,
    references: *std.ArrayList(Reference),
) anyerror!void {
    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    const name = nodeName(tree, node, document.source()) orelse return;

    var symbols = std.ArrayList(Reference).init(arena);
    defer symbols.deinit();

    try visibleFields(arena, document, node, &symbols);
    if (symbols.items.len == 0) {
        try visibleSymbols(arena, document, node, &symbols);
    }

    for (symbols.items) |symbol| {
        if (std.mem.eql(u8, name, symbol.name())) {
            try references.append(symbol);
            const parsed = try symbol.document.parseTree();
            if (!inFileRoot(parsed.tree, symbol.parent_declaration)) break;
        }
    }
}

fn inFileRoot(tree: Tree, node: u32) bool {
    const parent = tree.parent(node) orelse {
        // node is the file root
        return true;
    };
    return tree.tag(parent) == .file;
}

pub fn visibleFields(
    arena: std.mem.Allocator,
    document: *Document,
    start_node: u32,
    symbols: *std.ArrayList(Reference),
) !void {
    const lhs = lhs: {
        const parsed = try document.parseTree();
        const tree = parsed.tree;

        const tag = tree.tag(start_node);
        if (tag != .identifier and tag != .@".") return;

        const parent = tree.parent(start_node) orelse return;
        const selection = syntax.Selection.tryExtract(tree, parent) orelse return;
        var target = selection.get(.target, tree) orelse return;

        while (true) {
            switch (target.get(tree)) {
                .identifier => break :lhs target.node,
                .selection => |select| break :lhs select.nodeOf(.field, tree) orelse return,
                .array => |array| target = array.prefix(tree) orelse return,
                .number => return,
            }
        }
    };

    if (lhs == start_node) {
        // possible infinite loop if we are coming from `findDefinition`
        return;
    }

    var name_definitions = std.ArrayList(Reference).init(arena);
    try findDefinition(arena, document, lhs, &name_definitions);

    var references = std.ArrayList(Reference).init(document.workspace.allocator);
    defer references.deinit();

    for (name_definitions.items) |name_definition| {
        references.clearRetainingCapacity();
        try references.append(name_definition);

        var remaining_iterations: u32 = 16;

        while (references.popOrNull()) |reference| {
            if (remaining_iterations == 0 or references.items.len >= 128) break;
            remaining_iterations -= 1;

            const parsed = try reference.document.parseTree();
            const tree = parsed.tree;

            const typ = try typeOf(reference) orelse continue;

            const fields = if (typ.block_fields) |block_fields|
                block_fields
            else blk: {
                const specifier = typ.specifier orelse continue;
                switch (specifier) {
                    .struct_specifier => |struct_spec| {
                        if (struct_spec.get(.fields, tree)) |fields| {
                            break :blk fields.get(tree);
                        }
                    },
                    else => {
                        const identifier = specifier.underlyingName(tree) orelse continue;
                        if (identifier.node == start_node) continue;
                        try findDefinition(arena, reference.document, identifier.node, &references);
                    },
                }
                continue;
            };

            var iterator = fields.iterator();
            while (iterator.next(tree)) |field| {
                const variables = field.get(.variables, tree) orelse continue;
                var variable_iterator = variables.iterator();
                while (variable_iterator.next(tree)) |variable| {
                    const variable_name = variable.get(.name, tree) orelse continue;
                    const variable_identifier = variable_name.getIdentifier(tree) orelse continue;
                    try symbols.append(.{
                        .document = reference.document,
                        .node = variable_identifier.node,
                        .parent_declaration = field.node,
                    });
                }
            }
        }
    }
}

pub const Scope = struct {
    const ScopeId = u32;

    allocator: std.mem.Allocator,
    symbols: std.StringArrayHashMapUnmanaged(Symbol) = .{},
    active_scopes: std.ArrayListUnmanaged(ScopeId) = .{},
    next_scope: ScopeId = 0,

    const Symbol = struct {
        /// In which scope this item is valid.
        scope: ScopeId,
        /// The syntax node this name refers to.
        reference: Reference,
        /// Pointer to any shadowed symbols.
        shadowed: ?*Symbol = null,
    };

    /// Call this before entering a new scope.
    /// When a corresponding call to `end` is made, all symbols added within
    /// this scope are no longer visible.
    pub fn begin(self: *@This()) !void {
        try self.active_scopes.append(self.allocator, self.next_scope);
        self.next_scope += 1;
    }

    pub fn end(self: *@This()) void {
        _ = self.active_scopes.pop();
    }

    fn currentScope(self: *const @This()) ScopeId {
        const active = self.active_scopes.items;
        return active[active.len - 1];
    }

    pub fn add(self: *@This(), name: []const u8, reference: Reference) !void {
        const result = try self.symbols.getOrPut(self.allocator, name);
        errdefer self.symbols.swapRemoveAt(result.index);

        var shadowed: ?*Symbol = null;
        if (result.found_existing) {
            const copy = try self.allocator.create(Symbol);
            copy.* = result.value_ptr.*;
            shadowed = copy;
        }

        result.value_ptr.* = .{
            .scope = self.currentScope(),
            .reference = reference,
            .shadowed = shadowed,
        };
    }

    pub fn isActive(self: *const @This(), scope: ScopeId) bool {
        return std.mem.lastIndexOfScalar(ScopeId, self.active_scopes.items, scope) != null;
    }

    pub fn getVisible(self: *const @This(), symbols: *std.ArrayList(Reference)) !void {
        try symbols.ensureUnusedCapacity(self.symbols.count());
        for (self.symbols.values()) |*value| {
            var first_scope: ?u32 = null;
            var current: ?*Symbol = value;
            while (current) |symbol| : (current = symbol.shadowed) {
                if (!self.isActive(symbol.scope)) continue;

                // symbols from parent scopes are shadowed by the first scope
                first_scope = first_scope orelse symbol.scope;
                if (symbol.scope != first_scope) break;

                try symbols.append(symbol.reference);

                // only symbols in global scope can be overloaded
                if (symbol.scope != 0) break;
            }
        }
    }
};

/// Get a list of all symbols visible starting from the given syntax node.
pub fn visibleSymbols(
    arena: std.mem.Allocator,
    start_document: *Document,
    start_node: u32,
    symbols: *std.ArrayList(Reference),
) !void {
    var scope = Scope{ .allocator = arena };
    try scope.begin();
    defer scope.end();

    // collect global symbols:
    {
        var documents = try std.ArrayList(*Document).initCapacity(arena, 8);
        defer documents.deinit();

        try documents.append(start_document);
        try findIncludedDocumentsRecursive(arena, &documents);

        var documents_reverse = std.mem.reverseIterator(documents.items);
        while (documents_reverse.next()) |document| {
            try collectGlobalSymbols(&scope, document);
        }
    }

    {
        try scope.begin();
        defer scope.end();
        try collectLocalSymbols(arena, &scope, start_document, start_node);
        try scope.getVisible(symbols);
    }
}

fn collectLocalSymbols(
    arena: std.mem.Allocator,
    scope: *Scope,
    document: *Document,
    target_node: u32,
) !void {
    const parsed = try document.parseTree();
    const tree = parsed.tree;

    var path = try std.ArrayListUnmanaged(u32).initCapacity(arena, 8);
    defer path.deinit(arena);

    var closest_declaration: ?u32 = null;
    var vardecls_before: u32 = std.math.maxInt(u32);

    {
        var child = target_node;
        while (tree.parent(child)) |parent| : (child = parent) {
            try path.append(arena, child);

            if (closest_declaration == null) {
                if (syntax.VariableDeclaration.tryExtract(tree, parent)) |vardecl| {
                    var before = vardeclsBeforeInList(tree, vardecl) orelse 0;
                    before += @intFromBool(child == vardecl.nodeOf(.name, tree));
                    vardecls_before = before;
                }

                if (syntax.AnyDeclaration.match(tree, parent) != null) {
                    closest_declaration = parent;
                }
            }
        }
    }

    var i = path.items.len - 1;
    while (i >= 1) : (i -= 1) {
        const parent = path.items[i];
        const target_child = path.items[i - 1];

        const children = tree.children(parent);
        var current_child = children.start;
        while (current_child < target_child) : (current_child += 1) {
            try registerLocalDeclaration(scope, document, tree, current_child, .{});
        }

        if (current_child == closest_declaration) {
            try registerLocalDeclaration(scope, document, tree, current_child, .{
                .max_vardecl_count = vardecls_before,
            });
        }
    }
}

fn vardeclsBeforeInList(tree: Tree, vardecl: syntax.VariableDeclaration) ?u32 {
    const grandparent = tree.parent(vardecl.node) orelse return null;
    if (tree.tag(grandparent) != .variable_declaration_list) return null;
    const first = tree.children(grandparent).start;
    return vardecl.node - first;
}

fn registerLocalDeclaration(
    scope: *Scope,
    document: *Document,
    tree: Tree,
    node: u32,
    options: CollectOptions,
) !void {
    if (syntax.ExternalDeclaration.tryExtract(tree, node)) |declaration| {
        try collectDeclarationSymbols(scope, document, tree, declaration, options);
        return;
    }

    if (syntax.ParameterList.tryExtract(tree, node)) |parameters| {
        var iterator = parameters.iterator();
        while (iterator.next(tree)) |parameter| {
            const variable = parameter.get(.variable, tree) orelse continue;
            try registerVariables(scope, document, tree, .{ .one = variable }, parameter.node, .{});
        }
        return;
    }

    if (syntax.Parameter.tryExtract(tree, node)) |parameter| {
        const variable = parameter.get(.variable, tree) orelse return;
        try registerVariables(scope, document, tree, .{ .one = variable }, parameter.node, options);
    }

    if (syntax.ConditionList.tryExtract(tree, node)) |condition_list| {
        var statements = condition_list.iterator();
        while (statements.next(tree)) |statement| {
            switch (statement) {
                .declaration => |declaration| {
                    try collectDeclarationSymbols(
                        scope,
                        document,
                        tree,
                        .{ .variable = declaration },
                        .{},
                    );
                },
            }
        }
        return;
    }
}

fn collectGlobalSymbols(scope: *Scope, document: *Document) !void {
    const parsed = try document.parseTree();
    const tree = parsed.tree;

    const children = tree.children(tree.root);
    for (children.start..children.end) |child| {
        const global = syntax.ExternalDeclaration.tryExtract(tree, @intCast(child)) orelse continue;
        try collectDeclarationSymbols(scope, document, tree, global, .{});
    }
}

const CollectOptions = struct {
    max_vardecl_count: u32 = std.math.maxInt(u32),
};

fn collectDeclarationSymbols(
    scope: *Scope,
    document: *Document,
    tree: parse.Tree,
    external: syntax.ExternalDeclaration,
    options: CollectOptions,
) !void {
    switch (external) {
        .function => |function| {
            const ident = function.get(.identifier, tree) orelse return;
            try registerIdentifier(scope, document, tree, ident, function.node);
        },
        .variable => |declaration| {
            if (declaration.get(.specifier, tree)) |specifier| specifier: {
                if (specifier != .struct_specifier) break :specifier;
                const strukt = specifier.struct_specifier;
                const ident = strukt.get(.name, tree) orelse break :specifier;
                try registerIdentifier(scope, document, tree, ident, strukt.node);
            }

            const variables = declaration.get(.variables, tree) orelse return;
            try registerVariables(scope, document, tree, variables, declaration.node, options);
        },
        .block => |block| {
            if (block.get(.variable, tree)) |variable| {
                try registerVariables(scope, document, tree, .{ .one = variable }, block.node, options);
            } else {
                const fields = block.get(.fields, tree) orelse return;
                var field_iterator = fields.iterator();
                while (field_iterator.next(tree)) |field| {
                    const variables = field.get(.variables, tree) orelse continue;
                    try registerVariables(scope, document, tree, variables, field.node, .{});
                }
            }
        },
    }
}

fn registerIdentifier(
    scope: *Scope,
    document: *Document,
    tree: Tree,
    ident: syntax.Token(.identifier),
    declaration: u32,
) !void {
    try scope.add(ident.text(document.source(), tree), .{
        .document = document,
        .node = ident.node,
        .parent_declaration = declaration,
    });
}

fn registerVariables(
    scope: *Scope,
    document: *Document,
    tree: Tree,
    variables: syntax.Variables,
    declaration: u32,
    options: CollectOptions,
) !void {
    var max_vardecl_count = options.max_vardecl_count;
    var iterator = variables.iterator();
    while (iterator.next(tree)) |variable| {
        if (max_vardecl_count == 0) break;
        max_vardecl_count -= 1;

        const name = variable.get(.name, tree) orelse return;
        const ident = name.getIdentifier(tree) orelse return;
        try registerIdentifier(scope, document, tree, ident, declaration);
    }
}

/// Appends the set of documents which are visible (recursively) from any of the documents in the list.
fn findIncludedDocumentsRecursive(
    arena: std.mem.Allocator,
    documents: *std.ArrayList(*Document),
) !void {
    var i: usize = 0;
    while (i < documents.items.len) : (i += 1) {
        try findIncludedDocuments(arena, documents.items[i], documents);
    }
}

/// Appends the set of documents which are visible (directly) from the given document.
fn findIncludedDocuments(
    arena: std.mem.Allocator,
    start: *Document,
    documents: *std.ArrayList(*Document),
) !void {
    const parsed = try start.parseTree();

    const document_dir = std.fs.path.dirname(start.path) orelse return;

    for (parsed.tree.ignored()) |extra| {
        const line = start.source()[extra.start..extra.end];
        const directive = parse.parsePreprocessorDirective(line) orelse continue;
        switch (directive) {
            .include => |include| {
                var include_path = line[include.path.start..include.path.end];

                const is_relative = !std.fs.path.isAbsolute(include_path);

                var absolute_path = include_path;
                if (is_relative) absolute_path = try std.fs.path.join(arena, &.{ document_dir, include_path });
                defer if (is_relative) arena.free(absolute_path);

                const uri = try util.uriFromPath(arena, absolute_path);
                defer arena.free(uri);

                const included_document = start.workspace.getOrLoadDocument(.{ .uri = uri }) catch |err| {
                    std.log.err("could not open '{'}': {s}", .{
                        std.zig.fmtEscapes(uri),
                        @errorName(err),
                    });
                    continue;
                };

                for (documents.items) |document| {
                    if (document == included_document) {
                        // document has already been included.
                        break;
                    }
                } else {
                    try documents.append(included_document);
                }
            },
            else => continue,
        }
    }
}

fn nodeName(tree: Tree, node: u32, source: []const u8) ?[]const u8 {
    switch (tree.tag(node)) {
        .identifier => {
            const token = tree.token(node);
            return source[token.start..token.end];
        },
        .preprocessor => {
            const token = tree.token(node);
            const text = source[token.start..token.end];
            switch (parse.parsePreprocessorDirective(text) orelse return null) {
                .define => |define| return text[define.name.start..define.name.end],
                else => return null,
            }
        },
        else => return null,
    }
}

test "find definition local variable" {
    try expectDefinition(
        \\void main() {
        \\    int /*2*/x = 1;
        \\    /*1*/x += 2;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
    });
    try expectDefinition(
        \\void main() {
        \\    for (int /*2*/i = 0; i < 10; i++) {
        \\         /*1*/i += 1;
        \\    }
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
    });
    try expectDefinition(
        \\void main() {
        \\    int /*3*/foo;
        \\    {
        \\        float /*2*/foo;
        \\        /*1*/foo;
        \\    }
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
        .{ .source = "/*1*/", .target = "/*3*/", .should_exist = false },
    });
}

test "find definition parameter" {
    try expectDefinition(
        \\int bar(int /*2*/x) {
        \\    return /*1*/x;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
    });
    try expectDefinition(
        \\int foo(int /*2*/x) { return x; }
        \\int bar() {
        \\    return /*1*/x;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = false },
    });
}

test "find definition function" {
    try expectDefinition(
        \\void /*3*/foo(int x) {}
        \\void /*2*/foo() {}
        \\void main() {
        \\    /*1*/foo();
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
        .{ .source = "/*1*/", .target = "/*3*/", .should_exist = true },
    });
    try expectDefinition(
        \\void /*3*/foo() {}
        \\void main() {
        \\    int /*2*/foo = 123;
        \\    /*1*/foo();
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
        .{ .source = "/*1*/", .target = "/*3*/", .should_exist = false },
    });
}

test "find definition global" {
    try expectDefinition(
        \\layout(location = 1) uniform vec4 /*2*/color;
        \\void main() {
        \\    /*1*/color;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
    });
    try expectDefinition(
        \\layout(location = 1) uniform MyBlock { vec4 /*4*/color; } /*2*/my_block;
        \\void main() {
        \\    /*3*/color;
        \\    /*1*/my_block;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*2*/", .should_exist = true },
        .{ .source = "/*3*/", .target = "/*4*/", .should_exist = false },
    });
}

test "find definition field" {
    try expectDefinition(
        \\struct Foo { int /*1*/bar, /*2*/baz; };
        \\void main() {
        \\    Foo foo;
        \\    foo./*3*/bar;
        \\    foo./*4*/baz;
        \\}
    , &.{
        .{ .source = "/*3*/", .target = "/*1*/", .should_exist = true },
        .{ .source = "/*3*/", .target = "/*2*/", .should_exist = false },
        .{ .source = "/*4*/", .target = "/*1*/", .should_exist = false },
        .{ .source = "/*4*/", .target = "/*2*/", .should_exist = true },
    });
}

test "find definition field recursive" {
    try expectDefinition(
        \\struct Foo { int /*1*/foo; };
        \\struct Bar { Foo /*2*/bar; };
        \\void main() {
        \\    Bar baz;
        \\    baz./*3*/bar./*4*/foo;
        \\}
    , &.{
        .{ .source = "/*3*/", .target = "/*1*/", .should_exist = false },
        .{ .source = "/*3*/", .target = "/*2*/", .should_exist = true },
        .{ .source = "/*4*/", .target = "/*1*/", .should_exist = true },
        .{ .source = "/*4*/", .target = "/*2*/", .should_exist = false },
    });
}

test "find definition self" {
    try expectDefinition(
        \\void main(int /*1*/whatever) {
        \\    float /*2*/foo;
        \\}
    , &.{
        .{ .source = "/*1*/", .target = "/*1*/", .should_exist = true },
        .{ .source = "/*2*/", .target = "/*2*/", .should_exist = true },
    });
}

test "find definition self-multi" {
    try expectDefinition(
        \\void main() {
        \\    float /*1*/foo = 123, bar = /*2*/foo;
        \\}
    , &.{
        .{ .source = "/*2*/", .target = "/*1*/", .should_exist = true },
    });
}

test "find definition local shadowing" {
    try expectDefinition(
        \\void main() {
        \\    float /*1*/foo;
        \\    int /*2*/foo = /*3*/foo;
        \\}
    , &.{
        .{ .source = "/*3*/", .target = "/*1*/", .should_exist = true },
        .{ .source = "/*3*/", .target = "/*2*/", .should_exist = false },
    });
}

fn expectDefinition(
    source: []const u8,
    cases: []const struct {
        source: []const u8,
        target: []const u8,
        should_exist: bool,
    },
) !void {
    var workspace = try Workspace.init(std.testing.allocator);
    defer workspace.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "file://test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    var print_source = false;

    for (cases) |case| {
        const usage = cursors.get(case.source) orelse std.debug.panic("invalid cursor: {s}", .{case.source});
        const definition = cursors.get(case.target) orelse std.debug.panic("invalid cursor: {s}", .{case.source});

        var references = std.ArrayList(Reference).init(workspace.allocator);
        defer references.deinit();
        try findDefinition(arena.allocator(), document, usage.node, &references);

        var found_definition = false;
        for (references.items) |reference| {
            if (reference.document == document and reference.node == definition.node) {
                found_definition = true;
                break;
            }
        }

        if (case.should_exist and !found_definition) {
            std.log.err("definition not found: {s} -> {s}", .{ case.source, case.target });
            print_source = true;
        }

        if (!case.should_exist and found_definition) {
            std.log.err("unexpected definition: {s} -> {s}", .{ case.source, case.target });
            print_source = true;
        }
    }

    if (print_source) {
        std.debug.print("================\n{s}\n================\n", .{source});
    }
}

const Cursor = struct {
    node: u32,
};

fn findCursors(document: *Document) !std.StringArrayHashMap(Cursor) {
    const parsed = try document.parseTree();
    const tree = &parsed.tree;

    var cursors = std.StringArrayHashMap(Cursor).init(std.testing.allocator);
    errdefer cursors.deinit();

    for (parsed.ignored) |cursor| {
        for (tree.nodes.items(.span), tree.nodes.items(.tag), 0..) |token, tag, index| {
            if (tag.isSyntax()) continue;
            if (cursor.end == token.start) {
                try cursors.putNoClobber(
                    document.source()[cursor.start..cursor.end],
                    .{ .node = @intCast(index) },
                );
                break;
            }
        } else {
            std.debug.panic("cursor not found: \"{}\"", .{
                std.zig.fmtEscapes(document.source()[cursor.start..cursor.end]),
            });
        }
    }

    return cursors;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
