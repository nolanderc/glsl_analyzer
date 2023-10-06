const std = @import("std");
const parse = @import("parse.zig");
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
        return parsed.tree.nodes.items(.span)[self.node];
    }

    pub fn name(self: @This()) []const u8 {
        const s = self.span();
        return self.document.source()[s.start..s.end];
    }
};

pub const Type = struct {
    qualifiers: ?syntax.QualifierList = null,
    specifier: ?syntax.TypeSpecifier = null,
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

        if (data.type.qualifiers) |qualifiers| {
            try prettify(data.tree, data.source, writer, prettifyOptions(qualifiers.node));
        }
        if (data.type.specifier) |specifier| {
            if (data.type.qualifiers != null) try writer.writeByte(' ');
            try prettify(data.tree, data.source, writer, prettifyOptions(specifier.getNode()));
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
    const allocator = std.testing.allocator;

    var workspace = try Workspace.init(allocator);
    defer workspace.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "test.glsl", .version = 0 });
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

        try findDefinition(document, cursor.definition, &references);
        if (references.items.len != 1) return error.InvalidReference;
        const ref = references.items[0];

        const typ = try typeOf(ref) orelse return error.InvalidType;

        const found = try std.fmt.allocPrint(allocator, "{}", .{typ.format(tree, document.source())});
        defer allocator.free(found);

        try std.testing.expectEqualStrings(expected, found);
    }
}

// Given a node in the given parse tree, attempts to find the node(s) it references.
pub fn findDefinition(document: *Document, node: u32, references: *std.ArrayList(Reference)) !void {
    const workspace = document.workspace;
    const allocator = workspace.allocator;

    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    if (tree.tag(node) != .identifier) return;

    const identifier = tree.token(node);
    const name = document.source()[identifier.start..identifier.end];

    var symbols = std.ArrayList(Reference).init(allocator);
    defer symbols.deinit();

    try visibleSymbols(document, node, &symbols);

    for (symbols.items) |symbol| {
        const source = symbol.document.source();
        const parsed = try symbol.document.parseTree();
        if (isExpectedIdentifier(parsed.tree, symbol.node, source, name)) {
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

pub fn visibleFields(document: *Document, node: u32, symbols: *std.ArrayList(Reference)) !void {
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

    var name_definitions = std.ArrayList(Reference).init(document.workspace.allocator);
    defer name_definitions.deinit();
    try findDefinition(document, name, &name_definitions);
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
            const specifier = typ.specifier orelse continue;

            switch (specifier) {
                .struct_specifier => |struct_spec| {
                    const fields = struct_spec.get(.fields, tree) orelse continue;
                    var iterator = fields.get(tree).iterator();
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
                },
                else => {
                    const identifier = specifier.underlyingName(tree) orelse continue;
                    try findDefinition(reference.document, identifier.node, &references);
                },
            }
        }
    }
}

/// Get a list of all symbols visible starting from the given syntax node
pub fn visibleSymbols(
    start_document: *Document,
    start_node: u32,
    symbols: *std.ArrayList(Reference),
) !void {
    const workspace = start_document.workspace;
    const allocator = workspace.allocator;

    var visited_documents = std.AutoHashMap(*Document, void).init(allocator);
    defer visited_documents.deinit();

    var stack = std.ArrayList(struct { document: *Document, node: ?u32 }).init(allocator);
    defer stack.deinit();

    try stack.append(.{ .document = start_document, .node = start_node });
    try visited_documents.put(start_document, {});

    while (stack.popOrNull()) |current| {
        const document = current.document;
        const parse_tree = try document.parseTree();
        const tree = parse_tree.tree;
        const node = current.node orelse tree.rootIndex();

        try visibleSymbolsTree(document, tree.parent(node) orelse node, symbols);

        if (std.fs.path.dirname(document.uri)) |document_dir| {
            const node_start = tree.nodeSpanExtreme(node, .start);

            // parse include directives
            for (parse_tree.ignored.items) |ignored| {
                // only process directives before the node:
                if (ignored.end > node_start) break;

                const line = document.source()[ignored.start..ignored.end];
                const directive = parse.parsePreprocessorDirective(line) orelse continue;
                switch (directive) {
                    .include => |include| {
                        const relative_path = line[include.path.start..include.path.end];
                        const uri = if (std.fs.path.isAbsolute(relative_path))
                            try std.fs.path.join(allocator, &.{ "file://", relative_path })
                        else
                            try std.fs.path.join(allocator, &.{ document_dir, relative_path });
                        defer allocator.free(uri);

                        const included_document = workspace.getOrLoadDocument(.{ .uri = uri }) catch |err| {
                            std.log.err("could not open '{'}': {s}", .{
                                std.zig.fmtEscapes(uri),
                                @errorName(err),
                            });
                            continue;
                        };

                        const entry = try visited_documents.getOrPut(included_document);
                        if (entry.found_existing) continue;

                        try stack.append(.{ .document = included_document, .node = null });
                    },
                    else => continue,
                }
            }
        }
    }
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
            previous
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
                try findVisibleSymbols(document, tree, child, symbols, .{
                    .check_children = options.check_children or tag == .block_declaration,
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

fn isExpectedIdentifier(tree: Tree, index: u32, source: []const u8, name: []const u8) bool {
    const node = tree.nodes.get(index);
    const token = if (node.tag == .identifier) node.span else return false;
    return std.mem.eql(u8, source[token.start..token.end], name);
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
        \\layout(location = 1) uniform MyBlock { vec4 /*1*/color; } /*2*/my_block;
        \\void main() {
        \\    /*1*/color;
        \\    /*2*/my_block;
        \\}
    );
}

fn expectDefinitionIsFound(source: []const u8) !void {
    var workspace = try Workspace.init(std.testing.allocator);
    defer workspace.deinit();

    const document = try workspace.getOrCreateDocument(.{ .uri = "test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    for (cursors.values()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            var references = std.ArrayList(Reference).init(workspace.allocator);
            defer references.deinit();
            try findDefinition(document, usage, &references);
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

    const document = try workspace.getOrCreateDocument(.{ .uri = "test.glsl", .version = 0 });
    try document.replaceAll(source);

    var cursors = try findCursors(document);
    defer cursors.deinit();

    for (cursors.values()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            var references = std.ArrayList(Reference).init(workspace.allocator);
            defer references.deinit();
            try findDefinition(document, usage, &references);
            if (references.items.len != 0) {
                const ref = references.items[0];
                std.debug.print("found unexpected reference: {s}:{}", .{ ref.document.uri, ref.node });
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
        for (parsed.ignored.items) |cursor| {
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
