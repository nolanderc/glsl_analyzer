const std = @import("std");
const parse = @import("parse.zig");
const Tree = parse.Tree;
const Node = parse.Node;
const Workspace = @import("Workspace.zig");
const Document = @import("Document.zig");

// Given a node in the given parse tree, attempts to find the node it references.
pub fn findDefinition(document: *Document, node: u32) !?Reference {
    const workspace = document.workspace;
    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    if (tree.tag(node) != .identifier) return null;

    const identifier = tree.token(node);
    const name = document.source()[identifier.start..identifier.end];

    var symbols = std.ArrayList(Reference).init(workspace.allocator);
    defer symbols.deinit();
    try visibleSymbols(document, node, &symbols);

    for (symbols.items) |symbol| {
        const source = symbol.document.source();
        const parsed = try symbol.document.parseTree();
        if (isExpectedIdentifier(parsed.tree, symbol.node, source, name)) {
            return symbol;
        }
    } else {
        return null;
    }
}

/// Get a list of all symbols visible starting from the given syntax node
pub fn visibleSymbols(document: *Document, node: u32, symbols: *std.ArrayList(Reference)) !void {
    const parse_tree = try document.parseTree();
    const tree = parse_tree.tree;

    // walk the tree upwards until we find the containing declaration
    var current = node;
    while (true) {
        const parent = tree.parent(current) orelse break;
        const parent_node = tree.nodes.get(parent);
        const children = parent_node.getRange() orelse unreachable;

        // search for the identifier among the children
        var current_child = if (parent_node.tag == .file) children.end else current + 1;
        while (current_child > children.start) {
            current_child -= 1;
            try findVisibleSymbols(
                document,
                tree,
                current_child,
                symbols,
                .{ .check_children = parent_node.tag != .file },
            );
        }

        current = parent;
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
        .function_declaration, .struct_specifier, .variable_declaration => {
            const children = tree.children(index);
            var child = children.end;
            while (child > children.start) {
                child -= 1;

                switch (tree.tag(child)) {
                    .identifier => {
                        try symbols.append(.{
                            .document = document,
                            .node = child,
                            .parent_declaration = options.parent_declaration orelse index,
                        });
                        continue;
                    },
                    else => {},
                }

                try findVisibleSymbols(document, tree, child, symbols, options);
            }
        },
        .block => return,
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
                        .declaration, .parameter, .field_declaration, .function_declaration => index,
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

    var cursor_iter = cursors.valueIterator();
    while (cursor_iter.next()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            const ref = try findDefinition(document, usage) orelse return error.ReferenceNotFound;
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

    var cursor_iter = cursors.valueIterator();
    while (cursor_iter.next()) |cursor| {
        for (cursor.usages.slice()) |usage| {
            const ref = try findDefinition(document, usage) orelse return;
            std.debug.print("found unexpected reference: {s}:{}", .{ ref.document.uri, ref.node });
            return error.FoundUnexpectedReference;
        }
    }
}

const Cursor = struct {
    definition: u32,
    usages: std.BoundedArray(u32, 4) = .{},
};

fn findCursors(document: *Document) !std.StringHashMap(Cursor) {
    const parsed = try document.parseTree();
    const tree = &parsed.tree;

    var cursors = std.StringHashMap(Cursor).init(document.workspace.allocator);
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
