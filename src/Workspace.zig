const std = @import("std");
const util = @import("util.zig");
const lsp = @import("lsp.zig");
const Spec = @import("Spec.zig");
const parse = @import("parse.zig");
pub const Document = @import("Document.zig");

const Workspace = @This();

allocator: std.mem.Allocator,
arena_state: std.heap.ArenaAllocator.State,
spec: Spec,
builtin_completions: []const lsp.CompletionItem,

/// Documents in the workspace, accessed by their path.
documents: std.StringHashMapUnmanaged(*Document) = .{},

pub fn init(allocator: std.mem.Allocator) !@This() {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const spec = try Spec.load(arena.allocator());
    const builtin_completions = try builtinCompletions(arena.allocator(), &spec);

    return .{
        .allocator = allocator,
        .arena_state = arena.state,
        .spec = spec,
        .builtin_completions = builtin_completions,
    };
}

pub fn deinit(self: *Workspace) void {
    var entries = self.documents.iterator();
    while (entries.next()) |entry| {
        const document = entry.value_ptr.*;
        self.allocator.free(document.path);
        self.allocator.free(document.uri);
        document.deinit();
        self.allocator.destroy(document);
    }
    self.documents.deinit(self.allocator);
    self.arena_state.promote(self.allocator).deinit();
}

pub fn getDocument(self: *Workspace, document: lsp.TextDocumentIdentifier) !?*Document {
    const path = try util.pathFromUri(self.allocator, document.uri);
    defer self.allocator.free(path);
    return self.documents.get(path);
}

pub fn getOrCreateDocument(
    self: *Workspace,
    document: lsp.VersionedTextDocumentIdentifier,
) !*Document {
    const path = try util.pathFromUri(self.allocator, document.uri);
    errdefer self.allocator.free(path);

    const entry = try self.documents.getOrPut(self.allocator, path);
    if (entry.found_existing) {
        self.allocator.free(path);
    } else {
        errdefer self.documents.removeByPtr(entry.key_ptr);

        const new_document = try self.allocator.create(Document);
        errdefer self.allocator.destroy(new_document);

        const uri_clone = try self.allocator.dupe(u8, document.uri);
        errdefer self.allocator.free(uri_clone);

        entry.key_ptr.* = path;
        new_document.* = .{
            .uri = uri_clone,
            .path = path,
            .workspace = self,
            .version = document.version,
        };
        entry.value_ptr.* = new_document;
    }
    return entry.value_ptr.*;
}

pub fn getOrLoadDocument(
    self: *Workspace,
    document: lsp.TextDocumentIdentifier,
) !*Document {
    const path = try util.pathFromUri(self.allocator, document.uri);
    errdefer self.allocator.free(path);

    const entry = try self.documents.getOrPut(self.allocator, path);
    if (entry.found_existing) {
        self.allocator.free(path);
    } else {
        errdefer self.documents.removeByPtr(entry.key_ptr);

        const max_megabytes = 16;
        const contents = try std.fs.cwd().readFileAlloc(self.allocator, path, max_megabytes << 20);
        errdefer self.allocator.free(contents);

        const new_document = try self.allocator.create(Document);
        errdefer self.allocator.destroy(new_document);

        const uri_clone = try self.allocator.dupe(u8, document.uri);
        errdefer self.allocator.free(uri_clone);

        entry.key_ptr.* = path;
        new_document.* = .{
            .uri = uri_clone,
            .path = path,
            .workspace = self,
            .version = null,
            .contents = std.ArrayListUnmanaged(u8).fromOwnedSlice(contents),
        };
        entry.value_ptr.* = new_document;
    }
    return entry.value_ptr.*;
}

fn builtinCompletions(arena: std.mem.Allocator, spec: *const Spec) ![]lsp.CompletionItem {
    var completions = std.ArrayList(lsp.CompletionItem).init(arena);

    try completions.ensureUnusedCapacity(
        spec.types.len + spec.variables.len + spec.functions.len,
    );

    for (spec.types) |typ| {
        try completions.append(.{
            .label = typ.name,
            .kind = .class,
            .documentation = .{
                .kind = .markdown,
                .value = try std.mem.join(arena, "\n\n", typ.description),
            },
        });
    }

    keywords: for (spec.keywords) |keyword| {
        for (spec.types) |typ| {
            if (std.mem.eql(u8, keyword.name, typ.name)) {
                continue :keywords;
            }
        }

        try completions.append(.{
            .label = keyword.name,
            .kind = .keyword,
            .documentation = .{
                .kind = .markdown,
                .value = switch (keyword.kind) {
                    .glsl => "Available in standard GLSL.",
                    .vulkan => "Only available when targeting Vulkan.",
                    .reserved => "Reserved for future use.",
                },
            },
        });
    }

    for (spec.variables) |variable| {
        var signature = std.ArrayList(u8).init(arena);
        try signature.writer().print("{}", .{variable.modifiers});
        try signature.appendSlice(" ");
        try signature.appendSlice(variable.type);

        try completions.append(.{
            .label = variable.name,
            .labelDetails = .{
                .detail = try signature.toOwnedSlice(),
            },
            .kind = .variable,
            .documentation = if (variable.description) |desc| .{
                .kind = .markdown,
                .value = try std.mem.join(arena, "\n\n", desc),
            } else null,
        });
    }

    for (spec.functions) |function| {
        var anonymous_signature = std.ArrayList(u8).init(arena);
        var named_signature = std.ArrayList(u8).init(arena);
        try writeFunctionSignature(function, anonymous_signature.writer(), .{ .names = false });
        try writeFunctionSignature(function, named_signature.writer(), .{ .names = true });

        try completions.append(.{
            .label = function.name,
            .labelDetails = .{
                .detail = try anonymous_signature.toOwnedSlice(),
            },
            .kind = .function,
            .detail = try named_signature.toOwnedSlice(),
            .documentation = if (function.description) |desc| .{
                .kind = .markdown,
                .value = try std.mem.join(arena, "\n\n", desc),
            } else null,
        });
    }

    return completions.toOwnedSlice();
}

fn writeFunctionSignature(
    function: Spec.Function,
    writer: anytype,
    options: struct { names: bool },
) !void {
    try writer.writeAll(function.return_type);
    try writer.writeAll(" ");
    if (options.names) try writer.writeAll(function.name);
    try writer.writeAll("(");
    for (function.parameters, 0..) |param, i| {
        if (i != 0) try writer.writeAll(", ");
        if (param.optional) try writer.writeAll("[");
        if (param.modifiers) |modifiers| {
            try writer.print("{}", .{modifiers});
            try writer.writeAll(" ");
        }
        if (options.names) {
            const array_start = std.mem.indexOfScalar(u8, param.type, '[') orelse param.type.len;
            try writer.writeAll(param.type[0..array_start]);
            try writer.writeAll(" ");
            try writer.writeAll(param.name);
            try writer.writeAll(param.type[array_start..]);
        } else {
            try writer.writeAll(param.type);
        }
        if (param.optional) try writer.writeAll("]");
    }
    try writer.writeAll(")");
}
