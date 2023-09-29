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
    step.addAnonymousModule("glsl_spec.json", .{ .source_file = .{
        .path = b.pathFromRoot("spec/spec.json"),
    } });

    const options = b.addOptions();
    const build_root_path = try std.fs.path.resolve(b.allocator, &.{b.build_root.path orelse "."});
    options.addOption([]const u8, "build_root", build_root_path);
    step.addOptions("build_options", options);
}
