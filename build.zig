const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const test_filter = b.option([]const u8, "test-filter", "Filter for the tests");

    const main_tests_exe = b.addTestExe("test-exe", "src/main.zig");
    main_tests_exe.setBuildMode(mode);
    main_tests_exe.setTarget(target);
    main_tests_exe.setFilter(test_filter);
    main_tests_exe.install();

    const main_tests_exe_step = b.step("test-exe", "Produce an executable that runs the library tests.");
    main_tests_exe_step.dependOn(&main_tests_exe.step);

    const main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);
    main_tests.setTarget(target);
    main_tests.setFilter(test_filter);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
