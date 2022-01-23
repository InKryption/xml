const std = @import("std");

pub const debug_mode = @import("builtin").mode == .Debug;

pub inline fn DebugType(comptime T: type) type {
    return if (debug_mode) T else void;
}
pub inline fn debugValue(value: anytype) DebugType(@TypeOf(value)) {
    return if (debug_mode) value else {};
}
