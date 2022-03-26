const std = @import("std");
const builtin = @import("builtin");

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");

const TokenStream = @This();
src: []const u8 = &.{},
state: State = .{},

pub const Error = error{
    ExpectedLeftAngleBracket,
    ExpectedCharacterAfterLeftAngleBracket,
    ExpectedPiTargetName,
    ExpectedCharacterAfterLeftAngleBracketBang,
    ExpectedDoubleDashAfterLeftAngleBracketBang,
    ExpectedRightAngleBracketAfterDoubleDashInComment,
    ExpectedCommentClose,
};

pub const TokOrError = union(enum) {
    tok: Tok,
    err: Err,

    pub const Err = struct {
        code: Error,
        index: usize,
    };

    pub fn unwrap(self: TokOrError) Error!Tok {
        return switch (self) {
            .tok => |tok| tok,
            .err => |err| err.code,
        };
    }

    pub fn unwrapErr(self: TokOrError) ?Err {
        return switch (self) {
            .tok => null,
            .err => |err| err,
        };
    }
};

pub fn next(ts: *TokenStream) ?TokOrError {
    return if (nextImpl(&ts.state, ts.src)) |tok_or_null|
        if (tok_or_null) |tok| TokOrError{ .tok = tok } else null
    else |err|
        TokOrError{ .err = TokOrError.Err{
            .code = err,
            .index = ts.state.i,
        } };
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
    ts.* = .{
        .src = src,
    };
}

pub const Tok = struct {
    id: Id,
    index: usize,
    len: usize,

    pub const Id = enum {
        comment,

        pi_open,
        pi_tok,
        pi_str,
        pi_close,

        elem_open,
        elem_close,
        elem_close_inline,

        attr_name,
        attr_val_start,
        attr_val_text,
        attr_val_entref,
        attr_val_end,

        cdata,
        text,
        entref,
        whitespace,

        pub fn isElemClose(id: Id) bool {
            return switch (id) {
                .elem_close,
                .elem_close_inline,
                => true,
                else => false,
            };
        }

        pub fn isContent(id: Id) bool {
            return switch (id) {
                .cdata,
                .text,
                .entref,
                .whitespace,
                => true,
                else => false,
            };
        }

        pub fn minLength(id: Id) usize {
            return switch (id) {
                .comment => "<!--".len + "-->".len, // 7

                .pi_open => "<?".len + 1, // 3
                .pi_tok => 1,
                .pi_str => "\'".len + "\"".len, // 2
                .pi_close => "?>".len, // 2

                .elem_open => "<".len + 1, // 2
                .elem_close => "</".len + 1 + ">".len, // 4
                .elem_close_inline => "/>".len,

                .attr_name => 1,
                .attr_val_start => 1,
                .attr_val_text => 1,
                .attr_val_entref => "&".len + 1 + ";".len, // 3
                .attr_val_end => 1,

                .cdata => "<![CDATA[".len + "]]>".len, // 12
                .text => 1,
                .entref => "&".len + 1 + ";".len, // 3
                .whitespace => 1,
            };
        }

        /// `null` indicates unbounded.
        pub fn maxLength(id: Id) ?usize {
            return switch (id) {
                .comment => null,

                .pi_open => null,
                .pi_tok => null,
                .pi_str => null,
                .pi_close => "?>".len,

                .elem_open => null,
                .elem_close => null,
                .elem_close_inline => "/>".len,

                .attr_name => null,
                .attr_val_start => 1,
                .attr_val_text => null,
                .attr_val_entref => null,
                .attr_val_end => 1,

                .cdata => null,
                .text => null,
                .entref => null,
                .whitespace => null,
            };
        }
    };

    pub fn slice(tok: Tok, src: []const u8) []const u8 {
        if (builtin.mode == .Debug) {
            std.debug.assert(tok.len >= tok.id.minLength());
            std.debug.assert(tok.len <= tok.id.maxLength() orelse std.math.maxInt(usize));
        }
        return src[tok.index..][0..tok.len];
    }

    fn normal(id: Id, index: usize, len: usize) Tok {
        std.debug.assert(if (id.maxLength()) |max| max > id.minLength() else true);
        return Tok{
            .id = id,
            .index = index,
            .len = len,
        };
    }

    fn cannonical(id: Id, index: usize) Tok {
        std.debug.assert(if (id.maxLength()) |max| (max == id.minLength()) else false);
        return Tok{
            .id = id,
            .index = index,
            .len = id.maxLength().?,
        };
    }
};

const State = struct {
    i: usize = 0,
    progress: Progress = .begin,
    depth: usize = 0,

    const Progress = enum {
        begin,

        pi_open,

        end,
    };

    /// skips whitespace, and returns the number of
    /// characters skipped.
    inline fn skipWhitespace(state: *State, src: []const u8) usize {
        const start = state.i;
        while (state.i != src.len and utility.xml.isWhitespaceChar(src[state.i])) {
            state.i += 1;
        }
        return state.i - start;
    }

    inline fn tokenizeTopLevelComment(state: *State, src: []const u8) Error!?Tok {
        std.debug.assert(state.depth == 0);

        std.debug.assert(src[state.i - 1] == '<');
        std.debug.assert(src[state.i - 0] == '!');

        state.i += 1;
        if (state.i == src.len) {
            return Error.ExpectedCharacterAfterLeftAngleBracketBang;
        }
        if (src[state.i] != '-') {
            return Error.ExpectedDoubleDashAfterLeftAngleBracketBang;
        }

        return state.tokenizeComment(src);
    }

    inline fn tokenizeComment(state: *State, src: []const u8) Error!?Tok {
        std.debug.assert(src[state.i - 2] == '<');
        std.debug.assert(src[state.i - 1] == '!');
        std.debug.assert(src[state.i - 0] == '-');
        const start = state.i - 2;

        state.i += 1;
        if (state.i == src.len) {
            return Error.ExpectedDoubleDashAfterLeftAngleBracketBang;
        }
        if (src[state.i] != '-') {
            return Error.ExpectedDoubleDashAfterLeftAngleBracketBang;
        }

        return blk: while (state.i < src.len) : (state.i += std.unicode.utf8ByteSequenceLength(src[state.i]) catch unreachable) {
            if (std.mem.startsWith(u8, src[state.i..], "--")) {
                state.i += "--".len;
                if (state.i < src.len and src[state.i] == '>') {
                    state.i += 1;
                    break :blk Tok.normal(.comment, start, state.i - start);
                } else break :blk Error.ExpectedRightAngleBracketAfterDoubleDashInComment;
            }
        } else Error.ExpectedCommentClose;
    }

    inline fn tokenizePiTarget(state: *State, src: []const u8) Error!?Tok {
        std.debug.assert(src[state.i - 1] == '<');
        std.debug.assert(src[state.i - 0] == '?');
        const start = state.i - 1;

        state.i += 1;
        if (state.i == src.len) {
            return Error.ExpectedPiTargetName;
        }

        if (utility.xml.nameStartCharLengthAt(src, state.i)) |cp_len| {
            state.i += cp_len;
        } else {
            return Error.ExpectedPiTargetName;
        }

        while (state.i < src.len) {
            var cp_len: u3 = undefined;
            const cp = utility.codepointAt(src, state.i, &cp_len);

            if (utility.xml.isNameChar(cp)) {
                state.i += cp_len;
            } else break;
        }

        state.progress = .pi_open;
        return @as(Error!?Tok, Tok.normal(.pi_open, start, state.i - start));
    }
};

fn nextImpl(state: *State, src: []const u8) Error!?Tok {
    const progress_on_call: State.Progress = state.progress;
    defer std.debug.assert((progress_on_call == .end) or (state.progress != progress_on_call));

    const result: Error!?Tok = switch (state.progress) {
        .begin => begin: {
            _ = state.skipWhitespace(src);
            if (state.i == src.len) {
                break :begin null;
            }
            if (src[state.i] != '<') {
                break :begin Error.ExpectedLeftAngleBracket;
            }
            state.i += "<".len;
            if (state.i == src.len) {
                break :begin Error.ExpectedCharacterAfterLeftAngleBracket;
            }
            break :begin switch (src[state.i]) {
                '?' => state.tokenizePiTarget(src),
                '!' => state.tokenizeTopLevelComment(src),
                else => std.debug.todo(""),
            };
        },

        .pi_open => std.debug.todo(""),

        .end => null,
    };

    if (progress_on_call != .end and (std.meta.isError(result) or (result catch unreachable) == null)) {
        std.debug.assert(state.progress != .end);
        state.progress = .end;
    }

    return result;
}

const TestTokenStream = struct {
    ts: TokenStream = .{},

    fn reset(tts: *TestTokenStream, src: []const u8) validate_slice.ValidateSliceResult {
        return tts.ts.reset(src);
    }

    fn resetUnchecked(tts: *TestTokenStream, src: []const u8) void {
        return tts.ts.resetUnchecked(src);
    }

    fn next(tts: *TestTokenStream) ?TokOrError {
        return tts.ts.next();
    }

    fn expectNull(tts: *TestTokenStream) !void {
        const result = tts.next();
        try std.testing.expectEqual(@as(?TokOrError, null), result);
    }

    fn expectErr(tts: *TestTokenStream, err: Error) !void {
        const result = tts.next() orelse return error.TestExpectedError;
        try std.testing.expectError(err, result.unwrap());
    }
};

test {
    var tts = TestTokenStream{};

    inline for (.{
        "",
        " ",
        "\t",
        "\r",
        "\n",
        "\r\n",
        "    ",
        "\n\t",
    }) |empty| {
        try tts.reset(empty).unwrap();
        try tts.expectNull();

        try tts.reset(empty ++ "<").unwrap();
        try tts.expectErr(Error.ExpectedCharacterAfterLeftAngleBracket);
        try tts.expectNull();

        try tts.reset(empty ++ "<?").unwrap();
        try tts.expectErr(Error.ExpectedPiTargetName);
        try tts.expectNull();

        try tts.reset(empty ++ "<!").unwrap();
        try tts.expectErr(Error.ExpectedCharacterAfterLeftAngleBracketBang);
        try tts.expectNull();

        try tts.reset(empty ++ "<!-").unwrap();
        try tts.expectErr(Error.ExpectedDoubleDashAfterLeftAngleBracketBang);
        try tts.expectNull();
    }
}
