const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "glsl_analyzer",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    try attachModules(exe);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .name = "unit-tests",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    try attachModules(unit_tests);

    if (b.option(bool, "install-tests", "Install the unit tests in the `bin` folder") orelse false) {
        b.installArtifact(unit_tests);
    }

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}

fn attachModules(step: *std.Build.CompileStep) !void {
    const b = step.step.owner;

    step.linkLibC();

    const compressed_spec = try CompressStep.create(b, "spec.json.zlib", .{ .path = b.pathFromRoot("spec/spec.json") });
    step.addAnonymousModule("glsl_spec.json.zlib", .{ .source_file = compressed_spec.getOutput() });

    const options = b.addOptions();
    const build_root_path = try std.fs.path.resolve(
        b.allocator,
        &.{b.build_root.path orelse "."},
    );
    options.addOption([]const u8, "build_root", build_root_path);
    options.addOption([]const u8, "version", b.exec(&.{ "git", "describe", "--tags" }));
    step.addOptions("build_options", options);
}

const CompressStep = struct {
    step: std.Build.Step,
    generated_file: std.Build.GeneratedFile,
    input: std.Build.LazyPath,

    pub fn create(b: *std.Build, name: []const u8, path: std.Build.LazyPath) !*@This() {
        const self = try b.allocator.create(@This());
        self.* = .{
            .step = std.Build.Step.init(.{
                .id = .custom,
                .name = name,
                .owner = b,
                .makeFn = &make,
            }),
            .generated_file = .{ .step = &self.step },
            .input = path,
        };
        path.addStepDependencies(&self.step);
        return self;
    }

    pub fn getOutput(self: *@This()) std.Build.LazyPath {
        return .{ .generated = &self.generated_file };
    }

    fn make(step: *std.Build.Step, _: *std.Progress.Node) anyerror!void {
        const b = step.owner;
        const self = @fieldParentPtr(@This(), "step", step);
        const input_path = self.input.getPath(b);

        var man = b.cache.obtain();
        defer man.deinit();

        man.hash.add(@as(u32, 0x00000002));
        const input_index = try man.addFile(input_path, 16 << 20);

        const is_hit = try step.cacheHit(&man);

        const digest = man.final();

        const output_path = try b.cache_root.join(b.allocator, &.{ "o", &digest, step.name });
        self.generated_file.path = output_path;

        if (is_hit) return;

        const input_contents = man.files.items[input_index].contents.?;

        if (std.fs.path.dirname(output_path)) |dir| try b.cache_root.handle.makePath(dir);
        var output_file = b.cache_root.handle.createFile(output_path, .{}) catch |err| {
            std.log.err("could not open {s}: {s}", .{ output_path, @errorName(err) });
            return err;
        };
        defer output_file.close();

        var output_buffered = std.io.bufferedWriter(output_file.writer());
        var compress_stream = try std.compress.zlib.compressStream(b.allocator, output_buffered.writer(), .{});
        defer compress_stream.deinit();

        try compress_stream.writer().writeAll(input_contents);
        try compress_stream.finish();
        try output_buffered.flush();

        try step.writeManifest(&man);
    }
};
