const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Executable
    {
        const exe = try addExecutable(b, .{ .target = target, .optimize = optimize });
        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());

        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);
    }

    // Tests
    {
        const unit_tests = b.addTest(.{
            .name = "unit-tests",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        try attachModules(unit_tests);

        if (b.option(bool, "install-tests", "Install the unit tests in the `bin` folder") orelse false) {
            b.installArtifact(unit_tests);
        }

        const run_unit_tests = b.addRunArtifact(unit_tests);
        const test_step = b.step("test", "Run unit tests");
        test_step.dependOn(&run_unit_tests.step);
    }

    // Release
    {
        const target_triples = [_][]const u8{
            "x86_64-linux-musl",
            "aarch64-linux-musl",
            "x86_64-macos",
            "aarch64-macos",
            "x86_64-windows",
            "aarch64-windows",
        };
        const release_step = b.step("release", "Produce executables for targeted platforms");

        for (&target_triples) |triple| {
            const release_target = b.resolveTargetQuery(try std.Target.Query.parse(.{
                .arch_os_abi = triple,
                .cpu_features = "baseline",
            }));

            const exe = try addExecutable(b, .{ .target = release_target, .optimize = optimize });
            const install = b.addInstallArtifact(exe, .{
                .dest_dir = .{ .override = .{
                    .custom = b.pathJoin(&.{ triple, "bin" }),
                } },
            });

            release_step.dependOn(&install.step);
        }
    }
}

fn addExecutable(b: *std.Build, options: struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
}) !*std.Build.Step.Compile {
    const exe = b.addExecutable(.{
        .name = "glsl_analyzer",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = options.target,
            .optimize = options.optimize,
        }),
    });
    try attachModules(exe);
    return exe;
}

fn attachModules(step: *std.Build.Step.Compile) !void {
    const b = step.step.owner;

    step.linkLibC();

    step.root_module.addAnonymousImport("glsl_spec.json", .{ .root_source_file = b.path("spec/spec.json") });

    const options = b.addOptions();
    const build_root_path = try std.fs.path.resolve(
        b.allocator,
        &.{b.build_root.path orelse "."},
    );
    options.addOption([]const u8, "build_root", build_root_path);
    options.addOption([]const u8, "version", b.run(&.{ "git", "describe", "--tags", "--always" }));
    step.root_module.addOptions("build_options", options);
}
