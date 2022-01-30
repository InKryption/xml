const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const debug = std.debug;
const testing = std.testing;

const assert = debug.assert;
const todo = debug.todo;

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");
const TagTokenizer = @import("TagTokenizer.zig");

const TokenStream = @This();
frame: @Frame(TokenStream.tokenize) = undefined,
tok: *?Tok = undefined,

pub const Error = error{};

pub fn next(ts: *TokenStream) ?Tok {
    var tok: ?Tok = null;

    ts.tok = &tok;
    defer ts.tok = undefined;

    resume ts.frame;
    return tok;
}

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

pub const Tok = struct {
    index: usize,
    info: Info,

    pub fn init(index: usize, comptime id: Id, expr: anytype) Tok {
        return .{
            .index = index,
            .info = @unionInit(Info, @tagName(id), expr),
        };
    }

    pub const Id = meta.Tag(Info);
    pub const Info = union(enum) {
        err: Err,

        comment: Len,

        pub const Err = struct { code: Error };
        pub const Len = struct { len: usize };
    };
};

fn tokenize(ts: *TokenStream, src: []const u8) void {
    var i: usize = 0;
    var tt: TagTokenizer = .{};
    suspend {}

    _ = ts;
    _ = src;
    _ = i;
    _ = tt;

    tokenization: {
        i = utility.nextNonXmlWhitespaceCharIndexAfter(src[i..], i);
        tt.reset(src[i..]).unwrap() catch unreachable;
        if (tt.next()) |start_tag_tok| switch (start_tag_tok.info) {
            .err => todo("Implement 'err' branch."),

            .comment_start => {
                const next_tag_tok = tt.next() orelse todo("Implement this branch.");
                const last_tag_tok = tt.next() orelse todo("Implement this branch.");

                if (next_tag_tok.info != .comment_text) todo("Implement this branch.");
                if (last_tag_tok.info != .comment_end) todo("Implement this branch.");

                const len = len: {
                    var len: usize = 0;
                    len += start_tag_tok.expectedSlice().?.len;
                    len += next_tag_tok.info.comment_text.len;
                    len += last_tag_tok.expectedSlice().?.len;
                    break :len len;
                };
                suspend ts.tok.* = Tok.init(i, .comment, .{ .len = len });
                i += len;
            },

            .cdata_start => todo("Implement 'cdata_start' branch."),
            .pi_start => todo("Implement 'pi_start' branch."),
            .elem_open_start => todo("Implement 'elem_open_start' branch."),
            .elem_close_start => todo("Implement 'elem_close_start' branch."),

            else => didNotExpectTagToken(start_tag_tok.info),
        } else todo("Implement this branch");

        break :tokenization;
    }

    while (true) {
        suspend {}
    }
}

fn didNotExpectTagToken(tag_tok: TagTokenizer.Tok.Id) noreturn {
    if (@import("builtin").mode != .Debug)
        unreachable
    else {
        const format = "Didnt expect tag token '{s}'.\n";
        var buf: [std.fmt.count(format, .{utility.longestEnumName(TagTokenizer.Tok.Id)})]u8 = undefined;
        @panic(std.fmt.bufPrint(&buf, format, .{@tagName(tag_tok)}) catch unreachable);
    }
}

const tests = struct {
    const TestTokenStream = struct {
        ts: TokenStream = .{},
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

    ts.reset("<!-- foo -->").unwrap() catch unreachable;
    debug.print("\n{}\n", .{ts.next()});
}
