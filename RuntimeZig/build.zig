const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const enable_checks = b.option(bool, "checks", "Enable bounds checks") orelse
        (optimize == .Debug or optimize == .ReleaseSafe);
    const emit_asm = b.option(bool, "emit-asm", "Emit assembly for root module") orelse false;

    const root_module = b.createModule(.{
        .root_source_file = b.path("zorebo.zig"),
        .target = target,
        .optimize = optimize,
    });
    const options = b.addOptions();
    options.addOption(bool, "enable_checks", enable_checks);
    root_module.addOptions("build_options", options);

    const exe = b.addExecutable(.{
        .name = "zorebo",
        .root_module = root_module,
    });

    b.installArtifact(exe);

    if (emit_asm) {
        const obj = b.addObject(.{
            .name = "zorebo",
            .root_module = root_module,
        });
        const assem = obj.getEmittedAsm();
        const install_asm = b.addInstallFile(assem, "zorebo.s");
        b.getInstallStep().dependOn(&install_asm.step);
    }
}
