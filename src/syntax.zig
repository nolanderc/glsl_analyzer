//! Provides easy type-safe access and traversals of the nodes of a parse tree.

const syntax = @This();

const std = @import("std");
const parse = @import("parse.zig");
const Tree = parse.Tree;
const Tag = parse.Tag;

pub const File = ListExtractor(.file, null, ExternalDeclaration, null);

pub const ExternalDeclaration = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());
    function: FunctionDeclaration,
    variable: Declaration,
    block: BlockDeclaration,
};

pub const FunctionDeclaration = Extractor(.function_declaration, struct {
    qualifiers: QualifierList,
    specifier: TypeSpecifier,
    identifier: Token(.identifier),
    parameters: ParameterList,
    block: Block,
    semi: Token(.@";"),
});

pub const ParameterList = ListExtractor(.parameter_list, Token(.@"("), Parameter, Token(.@")"));

pub const Parameter = Extractor(.parameter, struct {
    qualifiers: QualifierList,
    specifier: TypeSpecifier,
    identifier: VariableDeclaration,
    comma: Token(.@","),
});

pub const BlockDeclaration = Extractor(.block_declaration, struct {
    qualifiers: QualifierList,
    specifier: TypeSpecifier,
    fields: FieldList,
    identifier: VariableDeclaration,
    semi: Token(.@";"),
});

pub const Declaration = Extractor(.declaration, struct {
    qualifiers: QualifierList,
    specifier: TypeSpecifier,
    variables: Variables,
    semi: Token(.@";"),
});

pub const Variables = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());
    one: VariableDeclaration,
    many: VariableDeclarationList,
};

pub const VariableDeclarationList = ListExtractor(.variable_declaration_list, null, VariableDeclaration, null);

pub const VariableDeclaration = Extractor(.variable_declaration, struct {
    name: VariableName,
    eq_token: Token(.@"="),
    initializer: Initializer,
    comma: Token(.@","),
});

pub const VariableName = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());

    identifier: Token(.identifier),
    array: ArraySpecifier(Token(.identifier)),

    pub fn identifier(self: @This()) ?Token(.identifier) {
        return switch (self) {
            .identifier => |token| token,
            .array => |array| array.prefix(),
        };
    }
};

pub const Initializer = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());

    list: InitializerList,
    expr: Expression,
};

pub const InitializerList = ListExtractor(.initializer_list, Token(.@"{"), Initializer, Token(.@"}"));

pub const QualifierList = ListExtractor(.type_qualifier_list, null, Qualifier, null);

pub const Qualifier = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());
};

pub const TypeSpecifier = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());

    identifier: Token(.identifier),
    array_specifier: ArraySpecifier(Token(.identifier)),
    struct_specifier: StructSpecifier,
};

pub const StructSpecifier = Extractor(.struct_specifier, struct {
    keyword_struct: Token(.keyword_struct),
    /// Struct fields may contain structs themselves, so we need lazy indirection here.
    fields: Lazy("FieldList"),
});

pub const FieldList = ListExtractor(.field_declaration_list, Token(.@"{"), Declaration, Token(.@"}"));

pub fn ArraySpecifier(comptime Inner: type) type {
    return ListExtractor(.array_specifier, Inner, Array, null);
}

pub const Array = Extractor(.array, struct {
    open: Token(.@"["),
    value: Expression,
    close: Token(.@"]"),
});

pub const Block = ListExtractor(.block, Token(.@"{"), Statement, Token(.@"}"));

pub const Statement = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());
};

pub const Expression = Lazy("ExpressionUnion");

pub const ExpressionUnion = union(enum) {
    pub usingnamespace UnionExtractorMixin(@This());

    identifier: Token(.identifier),
    number: Token(.number),
    array: ArraySpecifier(Expression),
};

fn Token(comptime tag: Tag) type {
    return struct {
        tree: *const Tree,
        node: u32,

        pub fn match(tree: *const Tree, node: u32) ?void {
            return if (tree.tag(node) == tag) {} else null;
        }

        pub fn extract(tree: *const Tree, node: u32, _: void) @This() {
            return .{ .tree = tree, .node = node };
        }
    };
}

fn Extractor(comptime expected_tag: Tag, comptime T: type) type {
    const fields = std.meta.fields(T);
    const FieldEnum = std.meta.FieldEnum(T);
    const FieldSet = std.EnumSet(FieldEnum);

    return struct {
        tree: *const Tree,
        node: u32,
        matches: FieldSet,

        pub fn match(tree: *const Tree, node: u32) ?void {
            return if (tree.tag(node) == expected_tag) {} else null;
        }

        pub fn extract(tree: *const Tree, node: u32, _: void) @This() {
            var children = tree.children(node);

            var matches = FieldSet.initEmpty();

            inline for (fields) |field| {
                while (children.start < children.end) : (children.start += 1) {
                    if (field.type.match(tree, children.start)) |_| {
                        matches.insert(@field(FieldEnum, field.name));
                    } else {
                        const tag = tree.tag(children.start);
                        if (tag == .invalid or tag == .unknown) continue;
                        break;
                    }
                }
            }

            return .{ .tree = tree, .node = node, .matches = matches };
        }

        pub fn get(self: @This(), comptime field: FieldEnum) ?std.meta.FieldType(T, field) {
            if (!self.matches.contains(field)) return null;

            const one = FieldSet.initOne(field).bits.mask;
            const bits = self.matches.bits.mask;
            const before = @popCount(bits & (one - 1));

            const node = self.tree.children(self.node).start + before;
            const Inner = std.meta.FieldType(T, field);
            const match_result = Inner.match(self.tree, node).?;
            return Inner.extract(self.tree, node, match_result);
        }
    };
}

fn ListExtractor(comptime tag: Tag, comptime Prefix: ?type, comptime Item: type, comptime Suffix: ?type) type {
    const PrefixMatch = if (Prefix) |P| MatchResult(P) else noreturn;
    const SuffixMatch = if (Suffix) |S| MatchResult(S) else noreturn;

    return struct {
        const Self = @This();

        tree: *const Tree,
        node: u32,
        prefix_match: ?PrefixMatch,
        suffix_match: ?SuffixMatch,
        items: parse.Range,

        pub fn match(tree: *const Tree, node: u32) ?void {
            return if (tag == tree.tag(node)) {} else null;
        }

        pub fn extract(tree: *const Tree, node: u32, _: void) @This() {
            const children = tree.children(node);

            const empty = children.start == children.end;

            const prefix_match: ?PrefixMatch = blk: {
                if (empty) break :blk null;
                if (Prefix) |P| break :blk P.match(tree, children.start);
                break :blk null;
            };

            const suffix_match: ?SuffixMatch = blk: {
                if (empty) break :blk null;
                if (Suffix) |S| break :blk S.match(tree, children.end - 1);
                break :blk null;
            };

            return .{
                .tree = tree,
                .node = node,
                .prefix_match = prefix_match,
                .suffix_match = suffix_match,
                .items = .{
                    .start = children.start + @intFromBool(prefix_match != null),
                    .end = children.end - @intFromBool(suffix_match != null),
                },
            };
        }

        pub usingnamespace if (Prefix) |PrefixType| struct {
            pub fn prefix(self: Self) ?PrefixType {
                return PrefixType.extract(self.tree, self.items.start - 1, self.prefix_match orelse return null);
            }
        } else struct {};

        pub usingnamespace if (Suffix) |SuffixType| struct {
            pub fn suffix(self: Self) ?SuffixType {
                return SuffixType.extract(self.tree, self.items.end, self.suffix_match orelse return null);
            }
        } else struct {};

        pub fn iterator(self: Self) Iterator {
            return .{
                .tree = self.tree,
                .items = self.items,
            };
        }

        pub const Iterator = struct {
            tree: *const Tree,
            items: parse.Range,

            pub fn next(self: *Iterator) ?Item {
                while (self.items.start < self.items.end) : (self.items.start += 1) {
                    if (Item.match(self.tree, self.items.start)) |res| {
                        return Item.extract(self.tree, self.items.start, res);
                    }
                }
                return null;
            }
        };
    };
}

fn UnionExtractorMixin(comptime Self: type) type {
    const fields = std.meta.fields(Self);

    var match_fields: [fields.len]std.builtin.Type.UnionField = undefined;

    for (&match_fields, fields) |*match_field, field| {
        match_field.* = field;
        match_field.type = MatchResult(field.type);
    }

    const MatchUnion = @Type(.{ .Union = .{
        .layout = .Extern,
        .fields = &match_fields,
        .tag_type = std.meta.Tag(Self),
        .decls = &.{},
    } });

    return struct {
        pub fn match(tree: *const Tree, node: u32) ?MatchUnion {
            inline for (fields) |field| {
                if (field.type.match(tree, node)) |value| {
                    return @unionInit(MatchUnion, field.name, value);
                }
            }
            return null;
        }

        pub fn extract(tree: *const Tree, node: u32, result: MatchUnion) Self {
            if (fields.len == 0) return undefined;

            switch (std.meta.activeTag(result)) {
                inline else => |res| {
                    const name = @tagName(res);
                    const Inner = @TypeOf(@field(@as(Self, undefined), name));
                    return @unionInit(Self, name, Inner.extract(tree, node, @field(result, name)));
                },
            }
        }
    };
}

/// Break type-level dependency cycles through lazy-evaluation indirection.
fn Lazy(comptime type_name: []const u8) type {
    return struct {
        tree: *const Tree,
        node: u32,

        fn Type() type {
            return @field(syntax, type_name);
        }

        pub fn match(tree: *const Tree, node: u32) ?void {
            return if (Type().match(tree, node) != null) {} else null;
        }

        pub fn extract(tree: *const Tree, node: u32, _: void) @This() {
            return .{ .tree = tree, .node = node };
        }

        pub fn get(self: @This()) Type() {
            const match_result = Type().match(self.tree, self.node).?;
            return Type().extract(self.tree, self.node, match_result);
        }
    };
}

fn MatchResult(comptime T: type) type {
    const match_fn_return = @typeInfo(@TypeOf(T.match)).Fn.return_type.?;
    return @typeInfo(match_fn_return).Optional.child;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
