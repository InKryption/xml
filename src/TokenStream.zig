const std = @import("std");

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");
const TagTokenizer = @import("TagTokenizer.zig");

const TokenStream = @This();
state: @Frame(TokenStream.tokenize) = undefined,

pub fn reset(ts: *TokenStream, src: []const u8) validate_slice.ValidateSliceResult {
    _ = ts;
    _ = src;
}

pub fn resetUnchecked(ts: *TokenStream, src: []const u8) void {
    ts.* = .{};
    ts.state = async ts.tokenize(src);
}

fn tokenize(ts: *TokenStream, src: []const u8) void {
    var i: usize = 0;
    var tt: TagTokenizer = .{};
    _ = ts;
    _ = src;
    _ = i;
    _ = tt;
    suspend {}

    tokenize: {
        // i = utility.nextNonWhitespaceCharIndexAfter(src, i);

        break :tokenize;
    }

    while (true) {
        suspend {}
    }
}
