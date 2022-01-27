const std = @import("std");

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");
const TagTokenizer = @import("TagTokenizer.zig");

const TokenStream = @This();
frame: @Frame(TokenStream.tokenize) = undefined,

pub fn reset(ts: *TokenStream, src: []const u8) validate_slice.ValidateSliceResult {
    const validation_result = validate_slice.validateUtf8Slice(src);
    switch (validation_result) {
        .ok => ts.resetUnchecked(src),
        .err => {},
    }
    return validation_result;
}

pub fn resetUnchecked(ts: *TokenStream, src: []const u8) void {
    ts.* = .{};
    ts.frame = async ts.tokenize(src);
}

pub const Tok = union(enum) {
    stub
};

fn tokenize(ts: *TokenStream, src: []const u8) void {
    var i: usize = 0;
    var tt: TagTokenizer = .{};
    _ = ts;
    _ = src;
    _ = i;
    _ = tt;
    suspend {}

    tokenize: {
        i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
        break :tokenize;
    }

    while (true) {
        suspend {}
    }
}

const tests = struct {
    const TestTokenStream = struct {
        ts: TagTokenizer = .{},
        src: []const u8 = &.{},

        fn reset(test_ts: *TestTokenStream, src: []const u8) validate_slice.ValidateSliceResult {
            test_ts.src = src;
            return test_ts.ts.reset(test_ts.src);
        }

        fn resetUnchecked(test_ts: *TestTokenStream, src: []const u8) void {
            test_ts.src = src;
            test_ts.ts.resetUnchecked(test_ts.src);
        }

        fn next(test_ts: *TestTokenStream) ?Tok {
            return test_ts.ts.next();
        }
    };
};

test {
    var ts = tests.TestTokenStream{};
    
    ts.reset("<?xml version=\"1.0\" encoding=\"UTF-8\"?>").unwrap() catch unreachable;
}
