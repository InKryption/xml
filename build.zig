const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);
    main_tests.setTarget(target);
    main_tests.setFilter(b.option([]const u8, "test-filter", "Filter for the tests"));

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
