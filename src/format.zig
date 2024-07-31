const std = @import("std");
const parse = @import("parse.zig");

pub const FormatOptions = struct {
    ignored: []const parse.Token = &.{},
    root_node: ?u32 = null,
    single_line: bool = false,
};

pub fn format(
    tree: parse.Tree,
    source: []const u8,
    writer: anytype,
    options: FormatOptions,
) !void {
    var inner_writer = Writer(@TypeOf(writer)){
        .child_writer = writer,
        .source = source,
        .ignored_tokens = options.ignored,
        .single_line = options.single_line,
    };

    const root = if (options.root_node) |root| blk: {
        inner_writer.last_emit = tree.nodeSpanExtreme(root, .start);
        break :blk root;
    } else tree.rootIndex();

    try formatNode(tree, root, &inner_writer);
}

fn Writer(comptime ChildWriter: type) type {
    return struct {
        const Self = @This();

        child_writer: ChildWriter,
        source: []const u8,
        indentation: usize = 0,
        needs_indent: bool = true,
        preceded_by_space: bool = true,
        pending_space: bool = false,

        single_line: bool,
        line_breaks: LineBreaks = .{},

        ignored_tokens: []const parse.Token,
        last_emit: u32 = 0,
        last_ignored: u32 = 0,

        current_line: u32 = 0,

        const LineBreaks = struct { min: u32 = 0, max: u32 = 2 };

        pub fn writeSpace(self: *Self) void {
            self.pending_space = true;
        }

        pub fn writeByte(self: *Self, byte: u8) !void {
            try self.writeToChildRaw(&.{byte});
        }

        pub fn writeAll(self: *Self, bytes: []const u8) !void {
            var start: usize = 0;
            while (std.mem.indexOfScalarPos(u8, bytes, start, '\n')) |end| {
                try self.writeLine(bytes[start..end]);
                try self.child_writer.writeByte(if (self.single_line) ' ' else '\n');
                start = end + 1;
                self.needs_indent = true;
                self.preceded_by_space = true;
                self.current_line += 1;
            }
            if (start < bytes.len) try self.writeLine(bytes[start..]);
        }

        fn writeLine(self: *Self, line: []const u8) !void {
            const trimmed = std.mem.trimRight(u8, line, &std.ascii.whitespace);
            if (trimmed.len == 0) return;
            try self.writeToChildRaw(trimmed);
        }

        fn writeToChildRaw(self: *Self, bytes: []const u8) !void {
            var extra_lines: u32 = 0;
            for (bytes) |byte| extra_lines += @intFromBool(byte == '\n');
            self.current_line += extra_lines;

            if (self.needs_indent) {
                self.needs_indent = false;
                try self.emitIndent();
            }

            if (self.pending_space) {
                self.pending_space = false;
                if (!self.preceded_by_space) {
                    try self.child_writer.writeByte(' ');
                }
            }

            try self.child_writer.writeAll(bytes);

            const last = bytes[bytes.len - 1];
            self.preceded_by_space = last == ' ' or last == '\n';
        }

        fn emitLeadingTokens(self: *Self, until: usize) !void {
            while (self.last_emit < until) {
                if (self.last_ignored >= self.ignored_tokens.len) break;

                const next = self.ignored_tokens[self.last_ignored];
                if (next.start > until) break;
                self.last_ignored += 1;

                try self.emitLeadingWhitespace(next.start, .{});
                if (!self.preceded_by_space) self.writeSpace();
                try self.writeAll(self.source[next.start..next.end]);

                if (std.mem.startsWith(u8, self.source[next.start..next.end], "/*")) {
                    self.writeSpace();
                } else {
                    self.line_breaks.min = 1;
                }

                self.last_emit = next.end;
            }
        }

        fn emitLeadingWhitespace(self: *Self, until: usize, line_breaks: LineBreaks) !void {
            const text = self.source[self.last_emit..until];
            var newlines: usize = 0;
            for (text) |ch| newlines += @intFromBool(ch == '\n');

            newlines = std.math.clamp(newlines, line_breaks.min, line_breaks.max);

            if (self.single_line) {
                if (newlines > 0 and !self.preceded_by_space) {
                    self.writeSpace();
                }
            } else {
                const newline_chars = [_]u8{'\n'} ** 4;
                try self.writeAll(newline_chars[0..newlines]);
            }

            self.line_breaks = .{};
        }

        pub fn emit(self: *Self, token: parse.Token) !void {
            try self.emitLeadingTokens(token.start);
            try self.emitLeadingWhitespace(token.start, self.line_breaks);
            try self.writeAll(self.source[token.start..token.end]);
            self.last_emit = token.end;
        }

        pub fn emitNoLeadingWhitespace(self: *Self, token: parse.Token) !void {
            self.line_breaks.max = @max(self.line_breaks.min, 0);
            try self.emit(token);
        }

        pub fn emitIndent(self: *Self) !void {
            try self.child_writer.writeByteNTimes(' ', self.indentation * 4);
            if (self.indentation > 0) self.preceded_by_space = true;
        }

        pub fn indent(self: *Self) void {
            self.indentation += 1;
        }

        pub fn dedent(self: *Self) void {
            self.indentation -= 1;
        }
    };
}

fn formatNode(tree: parse.Tree, current: usize, writer: anytype) !void {
    const old_indentation = writer.indentation;
    defer writer.indentation = old_indentation;
    defer writer.line_breaks = .{};

    switch (tree.tag(current)) {
        .file => {
            const children = tree.children(current);
            writer.line_breaks.max = 1;
            for (children.start..children.end) |child| {
                try formatNode(tree, child, writer);
            }
            try writer.emitLeadingTokens(writer.source.len);
            try writer.emitLeadingWhitespace(writer.source.len, .{ .min = 1, .max = 1 });
        },

        .block, .field_declaration_list, .initializer_list => {
            const children = tree.children(current);
            var inside_block = false;
            var has_children = false;

            for (children.start..children.end) |child| {
                const tag = tree.tag(child);

                if (tag == .@"}") {
                    const token = tree.token(child);
                    try writer.emitLeadingTokens(token.start);

                    writer.dedent();
                    if (has_children) {
                        writer.line_breaks.min = 1;
                        writer.line_breaks.max = 1;
                    } else {
                        writer.line_breaks.min = @max(0, writer.line_breaks.min);
                        writer.line_breaks.max = @max(0, writer.line_breaks.min);
                    }
                } else {
                    has_children = inside_block;
                }

                if (tag != .@"{" and tag != .@"}") {
                    writer.line_breaks.min = 1;
                }

                try formatNode(tree, child, writer);

                if (tag == .@"{") {
                    writer.indent();
                    inside_block = true;
                    writer.line_breaks.max = 1;
                }
            }
        },

        .parameter_list, .call, .condition_list => {
            const children = tree.children(current);
            const start_line = writer.current_line;

            for (children.start..children.end) |child| {
                switch (tree.tag(child)) {
                    .@"(" => {
                        try writer.emitNoLeadingWhitespace(tree.token(child));
                        writer.indent();
                    },
                    .@")" => {
                        writer.dedent();
                        if (writer.current_line == start_line) {
                            try writer.emitNoLeadingWhitespace(tree.token(child));
                        } else {
                            try writer.emit(tree.token(child));
                        }
                    },
                    else => try formatNode(tree, child, writer),
                }
            }
        },

        .@";",
        .@":",
        .@",",
        => {
            const token = tree.token(current);
            try writer.emitNoLeadingWhitespace(token);
            writer.writeSpace();
        },

        .if_branch, .else_branch => {
            const children = tree.children(current);
            for (children.start..children.end) |child| {
                const tag = tree.tag(child);
                if (child != children.start) writer.writeSpace();

                if (child + 1 == children.end and tag != .block) {
                    writer.indent();
                    try formatNode(tree, child, writer);
                    writer.dedent();
                } else {
                    try formatNode(tree, child, writer);
                }
            }
        },

        .statement => {
            const children = tree.children(current);
            for (children.start..children.end) |child| {
                const child_tag = tree.tag(child);
                if (child != children.start and needsLeadingSpace(child_tag)) writer.writeSpace();

                if (child + 1 == children.end and
                    child_tag != .block and
                    child_tag != .if_branch and
                    child_tag != .else_branch)
                {
                    writer.indent();
                    try formatNode(tree, child, writer);
                    writer.dedent();
                } else {
                    try formatNode(tree, child, writer);
                }

                if (child == children.start) {
                    switch (child_tag) {
                        .keyword_break,
                        .keyword_continue,
                        .keyword_do,
                        .keyword_for,
                        .keyword_while,
                        .keyword_switch,
                        .keyword_case,
                        .keyword_default,
                        .keyword_if,
                        .keyword_else,
                        .keyword_return,
                        => writer.line_breaks = .{ .min = 0, .max = 0 },
                        else => {},
                    }
                }
            }
        },

        // emit without spaces between childern
        .layout_qualifier,
        .prefix,
        .postfix,
        .array,
        .array_specifier,
        .selection,
        .parenthized,
        => {
            const children = tree.children(current);
            for (children.start..children.end) |child| {
                try formatNode(tree, child, writer);
            }
        },

        .conditional => {
            const children = tree.children(current);
            for (children.start..children.end) |child| {
                if (child != children.start) writer.writeSpace();
                try formatNode(tree, child, writer);
            }
        },

        // emit tokens separated by spaces
        inline else => |tag| {
            const operators = comptime parse.assignment_operators.unionWith(parse.infix_operators);

            if (comptime tag.isToken()) {
                try writer.emit(tree.token(current));
            } else {
                const children = tree.children(current);
                for (children.start..children.end) |child| {
                    const child_tag = tree.tag(child);
                    if (child != children.start and needsLeadingSpace(child_tag))
                        writer.writeSpace();

                    if (operators.contains(child_tag)) {
                        writer.line_breaks.min = @max(0, writer.line_breaks.min);
                        writer.line_breaks.max = 1;
                        writer.indentation = old_indentation + 1;
                    }

                    try formatNode(tree, child, writer);

                    if (operators.contains(child_tag)) {
                        writer.line_breaks = .{ .min = 0, .max = 1 };
                    }
                }
            }

            if (tag == .invalid) {
                writer.writeSpace();
            }
        },
    }
}

fn needsLeadingSpace(tag: parse.Tag) bool {
    return switch (tag) {
        .@":",
        .@";",
        .@",",
        .parameter_list,
        .array,
        => false,
        else => true,
    };
}

test "format simple" {
    try expectIsFormatted(
        \\#version 330 core
        \\
        \\layout(location = 0) in vec3 aPos; // the position variable
        \\
        \\out vec4 vertexColor; // specify a color output to the fragment shader
        \\
        \\void main() {
        \\    gl_Position = vec4(aPos, 1.0); // vec4 from vec3
        \\    vertexColor = vec4(0.5, 0.0, 0.0, 1.0); // set the output variable
        \\
        \\    int x = 1;
        \\    int y = 2;
        \\}
        \\
    );
}

test "format statements" {
    try expectFormat(
        \\void main() { int x = 1; int y = 2; }
        \\
        \\void main() { 
        \\
        \\
        \\int x = 1; 
        \\int y = 2;
        \\
        \\
        \\
        \\
        \\}
        \\
        \\void main() {}
        \\
    ,
        \\void main() {
        \\    int x = 1;
        \\    int y = 2;
        \\}
        \\
        \\void main() {
        \\    int x = 1;
        \\    int y = 2;
        \\}
        \\
        \\void main() {}
        \\
    );
}

test "format struct" {
    try expectFormat(
        \\struct Foo { vec3 position; vec4 color; } foo;
        \\
        \\struct Bar {} bar;
    ,
        \\struct Foo {
        \\    vec3 position;
        \\    vec4 color;
        \\} foo;
        \\
        \\struct Bar {} bar;
        \\
    );
}

test "format interface block" {
    try expectFormat(
        \\uniform MyBlock { float x; float y; } my_block;
    ,
        \\uniform MyBlock {
        \\    float x;
        \\    float y;
        \\} my_block;
        \\
    );
}

test "format operators" {
    try expectFormat(
        \\int x = 1+1;
        \\int y = -   1;
    ,
        \\int x = 1 + 1;
        \\int y = -1;
        \\
    );
}

test "format assignment" {
    try expectFormat(
        \\void main() {
        \\int x = 
        \\
        \\1+1;
        \\x = 
        \\
        \\-1;
        \\}
    ,
        \\void main() {
        \\    int x =
        \\        1 + 1;
        \\    x =
        \\        -1;
        \\}
        \\
    );
}

test "format multiline infix" {
    try expectIsFormatted(
        \\void main() {
        \\    int x = 1 + 2
        \\            + 3 * 4
        \\            + 5;
        \\}
        \\
    );
}

test "format call" {
    try expectFormat(
        \\vec4 x = vec4(
        \\  1,2  
        \\
        \\          ,
        \\              3      ,4,
        \\);
        \\
        \\float y = abs(123
        \\);
        \\
    ,
        \\vec4 x = vec4(
        \\        1, 2,
        \\        3, 4,
        \\    );
        \\
        \\float y = abs(123);
        \\
    );
}

test "format loops" {
    try expectIsFormatted(
        \\void main() {
        \\    for (int i = 0; i < 10; i++) print(i);
        \\    while (true) {
        \\        print(something);
        \\    }
        \\    do {
        \\        print(something);
        \\    } while (true);
        \\}
        \\
    );
}

test "format if/else" {
    try expectIsFormatted(
        \\void main() {
        \\    if (conditionA) {
        \\        // action A
        \\    } else if (conditionB) {
        \\        // action B
        \\    } else /* Condition C */ {
        \\        // action C
        \\    }
        \\
        \\    if (something)
        \\        return;
        \\    else
        \\        x += 1;
        \\}
        \\
    );
}

test "format function prototype" {
    try expectIsFormatted(
        \\int add(int, int);
        \\
    );
}

test "format qualifiers" {
    try expectIsFormatted(
        \\attribute vec4 color;
        \\varying vec4 color;
        \\
    );
}

test "format field selection" {
    try expectIsFormatted(
        \\void main() {
        \\    int gid = gl_GlobalInvocationId.x;
        \\}
        \\
    );
}

test "format ternary condition" {
    try expectIsFormatted(
        \\void main() {
        \\    int foo = a ? b : c;
        \\}
        \\
    );
}

test "format parenthized" {
    try expectIsFormatted(
        \\void main() {
        \\    int foo = (1 + 1);
        \\}
        \\
    );
}

test "format assign array" {
    try expectIsFormatted(
        \\void main() {
        \\    int foo = bar[123];
        \\}
        \\
    );
}

test "format array declaration" {
    try expectIsFormatted(
        \\void main() {
        \\    int[2] foo[123];
        \\}
        \\
    );
}

test "format only preprocessor" {
    try expectIsFormatted(
        \\#version 330
        \\
    );
    try expectIsFormatted(
        \\#version 330
        \\#define FOO
        \\#define BAR
        \\
    );
}

test "format only comments" {
    try expectIsFormatted(
        \\// hello world
        \\
    );
}

test "format multiline comment" {
    try expectIsFormatted(
        \\/*
        \\ * hello world
        \\ */
        \\
    );
}

test "format multiline define" {
    try expectIsFormatted(
        \\#define SNIPPET(x) \
        \\    do {\
        \\    x++;\
        \\    } while (0)
        \\
    );
}

test "format multiline array literal" {
    try expectIsFormatted(
        \\const float data[] = {
        \\        0.0,
        \\        1.0,
        \\        2.0,
        \\        3.0,
        \\        4.0,
        \\        5.0,
        \\    };
        \\
    );
}

fn expectIsFormatted(source: []const u8) !void {
    try expectFormat(source, source);
}

fn expectFormat(source: []const u8, expected: []const u8) !void {
    var ignored = std.ArrayList(parse.Token).init(std.testing.allocator);
    defer ignored.deinit();

    var diagnostics = std.ArrayList(parse.Diagnostic).init(std.testing.allocator);
    defer diagnostics.deinit();

    var tree = try parse.parse(std.testing.allocator, source, .{
        .ignored = &ignored,
        .diagnostics = &diagnostics,
    });
    defer tree.deinit(std.testing.allocator);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    try format(tree, source, buffer.writer(), .{ .ignored = ignored.items });

    std.testing.expectEqualStrings(expected, buffer.items) catch |err| {
        std.debug.print("\n========= parse tree ==========\n{}", .{tree.format(source)});

        if (diagnostics.items.len > 0) {
            std.debug.print("\n========= diagnostics ==========\n", .{});
            for (diagnostics.items) |diagnostic| {
                std.debug.print("{}:{}: {s}\n", .{
                    diagnostic.span.start,
                    diagnostic.span.end,
                    diagnostic.message,
                });
            }
        }

        return err;
    };
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
