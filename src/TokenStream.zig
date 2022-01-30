const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const debug = std.debug;
const testing = std.testing;

const assert = debug.assert;

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");
const TagTokenizer = @import("TagTokenizer.zig");

const TokenStream = @This();
frame: @Frame(TokenStream.tokenize) = undefined,
tok: *?Tok,

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
        if (tt.next()) |tok| switch (tok.info) {
            .err => debug.todo("Implement 'err' branch."),
            .comment_start => debug.todo("Implement 'comment_start' branch."),
            .cdata_start => debug.todo("Implement 'cdata_start' branch."),
            .pi_start => debug.todo("Implement 'pi_start' branch."),
            .elem_open_start => debug.todo("Implement 'elem_open_start' branch."),
            .elem_close_start => debug.todo("Implement 'elem_close_start' branch."),

            else => if (@import("builtin").mode == .Debug) {
                const longest_tok_name = comptime @tagName(std.enums.values(TagTokenizer.Tok.Id)[
                    std.sort.argMax(
                        TagTokenizer.Tok.Id,
                        std.enums.values(TagTokenizer.Tok.Id),
                        void{},
                        struct {
                            fn lessThan(_: void, lhs: TagTokenizer.Tok.Id, rhs: TagTokenizer.Tok.Id) bool {
                                return mem.lessThan(u8, @tagName(lhs), @tagName(rhs));
                            }
                        }.lessThan,
                    ).?
                ]);
                const format = "Didnt expect tag token '{s}'.";
                var buf: [std.fmt.count(format, .{longest_tok_name})]u8 = undefined;
                @panic(std.fmt.bufPrint(&buf, format, .{@tagName(tok.info)}) catch unreachable);
            } else unreachable,
        } else break :tokenization;
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
