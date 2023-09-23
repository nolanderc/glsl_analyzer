const std = @import("std");
const parse = @import("parse.zig");

pub const ExtraTokens = struct {
    ignored: []const parse.Token = &.{},
};

pub fn format(
    tree: parse.Tree,
    source: []const u8,
    writer: anytype,
    extra: ExtraTokens,
) !void {
    var inner_writer = Writer(@TypeOf(writer)){
        .child_writer = writer,
        .source = source,
        .extra = extra,
    };
    try formatNode(tree, tree.rootIndex(), &inner_writer);
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

        line_breaks: LineBreaks = .{},

        extra: ExtraTokens,
        last_emit: u32 = 0,
        last_ignored: u32 = 0,

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
                try self.child_writer.writeByte('\n');
                start = end + 1;
                self.needs_indent = true;
                self.preceded_by_space = true;
            }
            if (start < bytes.len) try self.writeLine(bytes[0..]);
        }

        fn writeLine(self: *Self, line: []const u8) !void {
            const trimmed = std.mem.trimRight(u8, line, &std.ascii.whitespace);
            if (trimmed.len == 0) return;
            try self.writeToChildRaw(trimmed);
        }

        fn writeToChildRaw(self: *Self, bytes: []const u8) !void {
            try self.flushIndent();
            if (self.pending_space) {
                self.pending_space = false;
                if (!self.preceded_by_space) try self.child_writer.writeByte(' ');
            }
            try self.child_writer.writeAll(bytes);
            self.preceded_by_space = bytes[bytes.len - 1] == ' ';
        }

        fn flushIndent(self: *Self) !void {
            if (self.needs_indent) {
                try self.emitIndent();
                self.needs_indent = false;
            }
        }

        fn emitLeadingTokens(self: *Self, until: u32) !void {
            while (self.last_emit < until) {
                if (self.last_ignored >= self.extra.ignored.len) break;

                const next = self.extra.ignored[self.last_ignored];
                if (next.start > until) break;
                self.last_ignored += 1;

                try self.emitLeadingWhitespace(next.start, .{});
                if (!self.preceded_by_space) self.writeSpace();
                try self.writeAll(self.source[next.start..next.end]);
                self.last_emit = next.end;
            }
        }

        fn emitLeadingWhitespace(self: *Self, until: usize, line_breaks: LineBreaks) !void {
            const text = self.source[self.last_emit..until];
            var newlines: usize = 0;
            for (text) |ch| newlines += @intFromBool(ch == '\n');

            newlines = std.math.clamp(newlines, line_breaks.min, line_breaks.max);

            const newline_chars = [_]u8{'\n'} ** 4;

            try self.writeAll(newline_chars[0..newlines]);

            self.line_breaks = .{};
        }

        pub fn emit(self: *Self, token: parse.Token) !void {
            try self.emitLeadingTokens(token.start);
            try self.emitLeadingWhitespace(token.start, self.line_breaks);
            try self.writeAll(self.source[token.start..token.end]);
            self.last_emit = token.end;
        }

        pub fn emitNoLeadingWhitespace(self: *Self, token: parse.Token) !void {
            try self.emitLeadingTokens(token.start);
            try self.writeAll(self.source[token.start..token.end]);
            self.last_emit = token.end;
        }

        pub fn emitIndent(self: *Self) !void {
            try self.child_writer.writeByteNTimes(' ', self.indentation);
            self.preceded_by_space = self.indentation > 0;
        }

        pub fn indent(self: *Self) void {
            self.indentation += 4;
        }

        pub fn dedent(self: *Self) void {
            self.indentation -= 4;
        }
    };
}

fn formatNode(tree: parse.Tree, current: usize, writer: anytype) !void {
    const old_indentation = writer.indentation;
    defer writer.indentation = old_indentation;
    defer writer.line_breaks = .{};

    switch (tree.nodes.get(current)) {
        .file => |children| {
            writer.line_breaks.max = 1;
            for (children.start..children.end) |child| {
                try formatNode(tree, child, writer);
            }
            try writer.emitLeadingWhitespace(writer.source.len, .{ .min = 1 });
        },

        .block, .field_declaration_list => |children| {
            var inside_block = false;
            var has_children = false;

            for (children.start..children.end) |child| {
                const tag = tree.tag(child);

                if (tag == .@"}") {
                    writer.dedent();
                    if (has_children) {
                        writer.line_breaks.min = 1;
                        writer.line_breaks.max = 1;
                    } else {
                        writer.line_breaks.min = 0;
                        writer.line_breaks.max = 0;
                    }
                } else {
                    has_children = inside_block;
                }

                try formatNode(tree, child, writer);

                if (tag == .@"{") {
                    writer.indent();
                    inside_block = true;
                    writer.line_breaks.max = 1;
                }
            }
        },

        // emit without spaces between childern
        .layout_qualifier, .prefix => |children| {
            for (children.start..children.end) |child| {
                try formatNode(tree, child, writer);
            }
        },

        .statement => |children| {
            writer.line_breaks.min = 1;
            for (children.start..children.end) |child| {
                try formatNode(tree, child, writer);
            }
        },

        .field_declaration => |children| {
            writer.line_breaks.min = 1;
            for (children.start..children.end) |child| {
                if (child != children.start and needsLeadingSpace(tree.tag(child)))
                    writer.writeSpace();
                try formatNode(tree, child, writer);
            }
        },

        .parameter_list,
        .call,
        => |children| {
            var first_parameter = true;
            for (children.start..children.end) |child| {
                const child_tag = tree.tag(child);

                if (child_tag == .parameter or child_tag == .argument) {
                    if (first_parameter) {
                        first_parameter = false;
                    } else {
                        writer.writeSpace();
                    }
                }

                if (child_tag == .@"(") writer.indent();
                if (child_tag == .@")") writer.dedent();

                try formatNode(tree, child, writer);
            }
        },

        .@";", .@"," => |token| try writer.emitNoLeadingWhitespace(token),

        // emit tokens separated by spaces
        inline else => |payload| {
            if (@TypeOf(payload) == parse.Token) {
                try writer.emit(payload);
            } else {
                for (payload.start..payload.end) |child| {
                    const tag = tree.tag(child);
                    if (child != payload.start and needsLeadingSpace(tag))
                        writer.writeSpace();

                    try formatNode(tree, child, writer);

                    if (parse.assignment_operators.contains(tag)) {
                        writer.line_breaks = .{ .min = 0, .max = 0 };
                    }
                }
            }
        },
    }
}

fn needsLeadingSpace(tag: parse.Node.Tag) bool {
    return switch (tag) {
        .@";",
        .@",",
        .parameter_list,
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
        \\    int x = 1 + 1;
        \\    x = -1;
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
    ,
        \\vec4 x = vec4(
        \\    1, 2,
        \\    3, 4,
        \\);
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
        std.debug.print("========= tree ==========\n{}", .{tree.format(source)});

        if (diagnostics.items.len > 0) {
            std.debug.print("========= diagnostics ==========\n", .{});
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
