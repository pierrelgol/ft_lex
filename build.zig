const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const no_bin = b.option(bool, "no-bin", "skip emitting binary") orelse false;

    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib_mod.addAnonymousImport("lexinputs", .{
        .root_source_file = b.path("test/inputs.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addAnonymousImport("lexinputs", .{
        .root_source_file = b.path("test/inputs.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("ft_lex_lib", lib_mod);

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "l",
        .root_module = lib_mod,
        .use_llvm = if (optimize == .Debug) true else false,
    });
    if (no_bin) {
        b.getInstallStep().dependOn(&lib.step);
    } else {
        b.installArtifact(lib);
    }

    const exe = b.addExecutable(.{
        .name = "ft_lex",
        .root_module = exe_mod,
        .use_llvm = if (optimize == .Debug) true else false,
    });
    if (no_bin) {
        b.getInstallStep().dependOn(&exe.step);
    } else {
        b.installArtifact(exe);
    }

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests = b.addTest(.{
        .root_module = lib_mod,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);

    const lib_check = b.addLibrary(.{
        .linkage = .static,
        .name = "l",
        .root_module = lib_mod,
        .use_llvm = if (optimize == .Debug) true else false,
    });

    const exe_check = b.addExecutable(.{
        .name = "ft_lex",
        .root_module = exe_mod,
        .use_llvm = if (optimize == .Debug) true else false,
    });

    const check_step = b.step("check", "for zls");
    check_step.dependOn(&exe_check.step);
    check_step.dependOn(&lib_check.step);
}
