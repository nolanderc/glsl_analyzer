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
        if (cursor.usages.len != 0) return error.DuplicateCursor;

        var references = std.ArrayList(Reference).init(allocator);
        defer references.deinit();

        try findDefinition(allocator, document, cursor.definition, &references);
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
) !void {
    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    const name = nodeName(tree, node, document.source()) orelse return;

    var symbols = std.ArrayList(Reference).init(arena);
    defer symbols.deinit();

    try visibleSymbols(arena, document, node, &symbols);

    for (symbols.items) |symbol| {
        const parsed = try symbol.document.parseTree();
        if (std.mem.eql(u8, name, symbol.name())) {
            try references.append(symbol);
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
    node: u32,
    symbols: *std.ArrayList(Reference),
) !void {
    const name = blk: {
        const parsed = try document.parseTree();
        const tag = parsed.tree.tag(node);
        if (tag != .identifier and tag != .@".") return;

        const parent = parsed.tree.parent(node) orelse return;
        const parent_tag = parsed.tree.tag(parent);
        if (parent_tag != .selection) return;

        const children = parsed.tree.children(parent);
        if (node == children.start) return;

        var first = children.start;
        while (parsed.tree.tag(first).isSyntax()) {
            const grand_children = parsed.tree.children(first);
            if (grand_children.start == grand_children.end) return;
            first = grand_children.start;
        }
        break :blk first;
    };

    var name_definitions = std.ArrayList(Reference).init(arena);
    try findDefinition(arena, document, name, &name_definitions);
    if (name_definitions.items.len == 0) return;

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
        for (self.active_scopes.items) |active| {
            if (scope == active) return true;
        }
        return false;
    }

    pub fn getVisible(self: *const @This(), symbols: *std.ArrayList(Reference)) !void {
        try symbols.ensureUnusedCapacity(self.symbols.count());
        for (self.symbols.values()) |*value| {
            var current: ?*Symbol = value;
            while (current) |symbol| : (current = symbol.shadowed) {
                if (!self.isActive(symbol.scope)) continue;
                try symbols.append(symbol.reference);
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
    _ = start_node;
    var scope = Scope{ .allocator = arena };
    try scope.begin();
    defer scope.end();

    // collect global symbols:
    {
        var documents = try std.ArrayList(*Document).initCapacity(arena, 8);
        defer documents.deinit();

        try documents.append(start_document);
        try findIncludedDocumentsRecursive(arena, &documents);

        for (documents.items) |document| {
            try collectGlobalSymbols(&scope, document);
        }
    }

    try scope.getVisible(symbols);
}

pub fn collectGlobalSymbols(scope: *Scope, document: *Document) !void {
    const parsed = try document.parseTree();
    const tree = parsed.tree;

    const children = tree.children(tree.root);
    for (children.start..children.end) |child| {
        const global = syntax.AnyDeclaration.tryExtract(tree, @intCast(child)) orelse continue;
        try collectSymbols(scope, document, tree, global);
    }
}

fn collectSymbols(
    scope: *Scope,
    document: *Document,
    tree: parse.Tree,
    any: syntax.AnyDeclaration,
) !void {
    switch (any) {
        .function => |function| {
            const ident = function.get(.identifier, tree) orelse return;
            try scope.add(ident.text(document.source(), tree), .{
                .document = document,
                .node = ident.node,
                .parent_declaration = function.node,
            });
        },
        .variable => |declaration| {
            const variables = declaration.get(.variables, tree) orelse return;
            var iterator = variables.iterator();
            while (iterator.next(tree)) |variable| {
                const name = variable.get(.name, tree) orelse return;
                const ident = name.getIdentifier(tree) orelse return;
                try scope.add(ident.text(document.source(), tree), .{
                    .document = document,
                    .node = ident.node,
                    .parent_declaration = declaration.node,
                });
            }
        },
        else => {
            std.log.warn("TODO: collect symbols from {s}", .{@tagName(any)});
        },
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

/// Splits the list into two partitions: the first with all unique symbols, and the second with all duplicates.
/// Returns the number of unique symbols.
fn partitonUniqueSymbols(
    allocator: std.mem.Allocator,
    symbols: []Reference,
    node_count: usize,
) !usize {
    // we want to keep the most recent symbol since its parent declaration will
    // be higher up the tree, so begin by reversing the list (we keep the first
    // unique value)
    std.mem.reverse(Reference, symbols);

    var visited = try std.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
    defer visited.deinit(allocator);

    var write: usize = 0;

    for (symbols) |*symbol| {
        if (visited.isSet(symbol.node)) continue;
        visited.set(symbol.node);
        std.mem.swap(Reference, &symbols[write], symbol);
        write += 1;
    }

    // Restore the order the nodes were visited.
    std.mem.reverse(Reference, symbols[0..write]);

    return write;
}

fn visibleSymbolsTree(document: *Document, start_node: u32, symbols: *std.ArrayList(Reference)) !void {
    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    // walk the tree upwards until we find the containing declaration
    var current = start_node;
    var previous = start_node;
    while (true) : ({
        previous = current;
        current = tree.parent(current) orelse break;
    }) {
        const children = tree.children(current);

        const tag = tree.tag(current);

        var current_child = if (tag == .file)
            children.end
        else if (previous != current)
            previous + 1
        else
            continue;

        // search for the identifier among the children
        while (current_child > children.start) {
            current_child -= 1;
            try findVisibleSymbols(
                document,
                tree,
                current_child,
                symbols,
                .{ .check_children = tag != .file },
            );
        }
    }
}

fn findVisibleSymbols(
    document: *Document,
    tree: Tree,
    index: u32,
    symbols: *std.ArrayList(Reference),
    options: struct {
        check_children: bool = true,
        parent_declaration: ?u32 = null,
    },
) !void {
    switch (tree.tag(index)) {
        .function_declaration => {
            if (syntax.FunctionDeclaration.tryExtract(tree, index)) |function| {
                if (function.get(.identifier, tree)) |identifier| {
                    try symbols.append(.{
                        .document = document,
                        .node = identifier.node,
                        .parent_declaration = index,
                    });
                }
            }

            const children = tree.children(index);
            var child = children.end;
            while (child > children.start) {
                child -= 1;
                try findVisibleSymbols(document, tree, child, symbols, options);
            }
        },
        .struct_specifier, .variable_declaration => {
            const children = tree.children(index);
            var child = children.end;
            while (child > children.start) {
                child -= 1;

                if (syntax.VariableName.tryExtract(tree, child)) |name| {
                    const identifier = name.getIdentifier(tree) orelse continue;
                    try symbols.append(.{
                        .document = document,
                        .node = identifier.node,
                        .parent_declaration = options.parent_declaration orelse index,
                    });
                    continue;
                }

                try findVisibleSymbols(document, tree, child, symbols, options);
            }
        },
        .block, .statement => return,
        else => |tag| {
            if (tag.isToken()) return;

            if (!options.check_children) {
                if (tag == .parameter_list or tag == .field_declaration_list) {
                    return;
                }
            }

            const children = tree.children(index);
            var child = children.end;
            while (child > children.start) {
                child -= 1;

                var check_children = options.check_children;
                if (syntax.BlockDeclaration.tryExtract(tree, child)) |block| {
                    if (block.get(.variable, tree) == null) {
                        // interface block without name:
                        check_children = true;
                    } else {
                        check_children = false;
                    }
                }

                try findVisibleSymbols(document, tree, child, symbols, .{
                    .check_children = check_children,
                    .parent_declaration = switch (tag) {
                        .declaration,
                        .parameter,
                        .function_declaration,
                        .block_declaration,
                        .struct_specifier,
                        => index,
                        else => options.parent_declaration,
                    },
                });
            }
        },
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
    try expectDefinitionIsFound(
        \\void main() {
        \\    int /*1*/x = 1;
        \\    /*1*/x += 2;
        \\}
    );
    try expectDefinitionIsFound(
        \\void main() {
        \\    for (int /*1*/i = 0; i < 10; i++) {
        \\         /*1*/i += 1;
        \\    }
        \\}
    );
}

test "find definition parameter" {
    try expectDefinitionIsFound(
        \\int bar(int /*1*/x) {
        \\    return /*1*/x;
        \\}
    );
    try expectDefinitionIsNotFound(
        \\int foo(int /*1*/x) { return x; }
        \\int bar() {
        \\    return /*1*/x;
        \\}
    );
}

test "find definition function" {
    try expectDefinitionIsFound(
        \\void /*1*/foo() {}
        \\void main() {
        \\    /*1*/foo();
        \\}
    );
    try expectDefinitionIsFound(
        \\void foo() {}
        \\void main() {
        \\    int /*1*/foo = 123;
        \\    /*1*/foo();
        \\}
    );
}

test "find definition global" {
    try expectDefinitionIsFound(
        \\layout(location = 1) uniform vec4 /*1*/color;
        \\void main() {
        \\    /*1*/color;
        \\}
    );
    try expectDefinitionIsFound(
        \\layout(location = 1) uniform MyBlock { vec4 color; } /*1*/my_block;
        \\void main() {
        \\    color;
        \\    /*1*/my_block;
        \\}
    );
    try expectDefinitionIsNotFound(
        \\layout(location = 1) uniform MyBlock { vec4 /*1*/color; } my_block;
        \\void main() {
        \\    /*1*/color;
        \\    my_block;
        \\}
    );
}

fn expectDefinitionIsFound(source: []const u8) !void {
    var workspace = try Workspace.init(std.testing.allocator);
    defer workspace.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "file://test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    for (cursors.values()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            var references = std.ArrayList(Reference).init(workspace.allocator);
            defer references.deinit();
            try findDefinition(arena.allocator(), document, usage, &references);
            if (references.items.len == 0) return error.ReferenceNotFound;
            if (references.items.len > 1) return error.MultipleDefinitions;
            const ref = references.items[0];
            try std.testing.expectEqual(document, ref.document);
            try std.testing.expectEqual(cursor.definition, ref.node);
        }
    }
}

fn expectDefinitionIsNotFound(source: []const u8) !void {
    var workspace = try Workspace.init(std.testing.allocator);
    defer workspace.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "file://test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    for (cursors.values()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            var references = std.ArrayList(Reference).init(workspace.allocator);
            defer references.deinit();
            try findDefinition(arena.allocator(), document, usage, &references);
            if (references.items.len != 0) {
                const ref = references.items[0];
                std.debug.print("found unexpected reference: {s}:{}\n", .{ ref.document.path, ref.node });
                return error.FoundUnexpectedReference;
            }
        }
    }
}

const Cursor = struct {
    definition: u32,
    usages: std.BoundedArray(u32, 4) = .{},
};

fn findCursors(document: *Document) !std.StringArrayHashMap(Cursor) {
    const parsed = try document.parseTree();
    const tree = &parsed.tree;

    var cursors = std.StringArrayHashMap(Cursor).init(document.workspace.allocator);
    errdefer cursors.deinit();

    for (0..tree.nodes.len) |index| {
        const node = tree.nodes.get(index);
        const token = node.getToken() orelse continue;
        for (parsed.ignored) |cursor| {
            if (cursor.end == token.start) {
                const result = try cursors.getOrPut(
                    document.source()[cursor.start..cursor.end],
                );
                if (result.found_existing) {
                    try result.value_ptr.usages.append(@intCast(index));
                } else {
                    result.value_ptr.* = .{ .definition = @intCast(index) };
                }
            }
        }
    }

    return cursors;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
