const std = @import("std");

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
    pub const Id = std.meta.Tag(Info);
    index: usize,
    info: Info,

    pub fn init(index: usize, comptime id: Tok.Id, expr: anytype) Tok {
        return Tok{
            .index = index,
            .info = @unionInit(Info, @tagName(id), expr),
        };
    }

    pub const Info = union(enum) {
        /// indicates an error in the source
        err: Err,

        /// indicates {' ', '\t', '\n', '\r'}+
        whitespace: Len,

        /// indicates '<?{name}'
        pi: Len,
        /// indicates a non-string token following 'pi_target', 'pi_str', or 'pi_tok'
        pi_tok: Len,
        /// indicates a string following 'pi_target', 'pi_tok', or 'pi_str'
        pi_str: Len,

        /// indicates '<!--{text}-->'
        comment: Len,

        /// indicates '<![CDATA[{text}]]>'
        cdata: Len,

        /// indicates '<{name}'
        elem_open: Len,
        /// indicates either '</{name}{s}?>' or '/>'; in the latter case,
        /// name_index and name_len fields refer to the previous 'elem_open' Tok.
        elem_close: ElementClose,

        /// refers to '{name}' following 'elem_open', 'attr_val_text' or 'attr_val_entref'.
        attr_name: Len,
        /// refers to '{text}' following 'attr_name' or 'attr_val_entref'.
        attr_val_text: Len,
        /// refers to '&{id};' following 'attr_name' or 'attr_val_text'.
        attr_val_entref: Len,

        pub const Err = struct { code: Error };
        pub const Len = struct { len: usize };
        pub const ElementClose = struct {
            len: usize,
            name_index: usize,
            name_len: usize,
        };
    };

    pub fn slice(tok: Tok, src: []const u8) []const u8 {
        return src[tok.index .. tok.index + tok.len()];
    }

    pub fn piTarget(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .pi) return null;
        return tok.slice(src)["<?".len..];
    }

    pub fn piStrText(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .pi_str) return null;
        const whole = tok.slice(src);
        return whole[1 .. whole.len - 1];
    }

    pub fn commentText(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .comment) return null;
        const whole = tok.slice(src);
        return whole["<!--".len .. whole.len - "-->".len];
    }

    pub fn cdataText(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .cdata) return null;
        const whole = tok.slice(src);
        return whole["<![CDATA[".len .. whole.len - "]]>".len];
    }

    pub fn elemOpenName(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .elem_open) return null;
        return tok.slice(src)["<".len..];
    }

    pub fn elemCloseName(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .elem_close) return null;
        const start = tok.info.elem_close.name_index;
        const end = start + tok.info.elem_close.name_len;
        return src[start..end];
    }

    pub fn len(tok: Tok) usize {
        return switch (tok.info) {
            .err => unreachable,
            .whitespace => |whitespace| whitespace.len,
            .pi => |pi| pi.len,
            .pi_tok => |pi_tok| pi_tok.len,
            .pi_str => |pi_str| pi_str.len,
            .comment => |comment| comment.len,
            .cdata => |cdata| cdata.len,
            .elem_open => |elem_open| elem_open.len,
            .elem_close => |elem_close| elem_close.len,
            .attr_name => |attr_name| attr_name.len,
            .attr_val_text => |attr_val_text| attr_val_text.len,
            .attr_val_entref => |attr_val_entref| attr_val_entref.len,
        };
    }
};

fn tokenize(ts: *TokenStream, src: []const u8) void {
    var i: usize = 0;
    var tag_tokenizer: TagTokenizer = .{};
    suspend {}

    _ = ts;
    _ = src;
    _ = i;
    _ = tag_tokenizer;

    tokenization: {
        i = utility.nextNonXmlWhitespaceCharIndexAfter(src[i..], i);

        while (true) {
            tag_tokenizer.reset(src[i..]).unwrap() catch unreachable;
            if (tag_tokenizer.next()) |start_tag_tok| switch (start_tag_tok.info) {
                .err => std.debug.todo("Implement 'err' branch."),

                .comment_start => {
                    const next_tag_tok = tag_tokenizer.next() orelse std.debug.todo("Implement this branch.");
                    const last_tag_tok = tag_tokenizer.next() orelse std.debug.todo("Implement this branch.");

                    if (next_tag_tok.info != .comment_text) std.debug.todo("Implement this branch.");
                    if (last_tag_tok.info != .comment_end) std.debug.todo("Implement this branch.");

                    const len = len: {
                        var len: usize = 0;
                        len += start_tag_tok.info.cannonicalSlice().?.len;
                        len += next_tag_tok.info.comment_text.len;
                        len += last_tag_tok.info.cannonicalSlice().?.len;
                        break :len len;
                    };
                    suspend ts.tok.* = Tok.init(i, .comment, .{ .len = len });
                    i += len;
                },

                .cdata_start => std.debug.todo("Implement 'cdata_start' branch."),
                .pi_start => std.debug.todo("Implement 'pi_start' branch."),
                .elem_open_start => std.debug.todo("Implement 'elem_open_start' branch."),
                .elem_close_start => std.debug.todo("Implement 'elem_close_start' branch."),

                else => didNotExpectTagToken(start_tag_tok.info),
            } else std.debug.todo("Implement this branch");
        }

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

        fn expectComment(test_ts: *TestTokenStream, text: []const u8) !void {
            const tok = test_ts.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.comment, tok.info);
            try std.testing.expectEqualStrings(text, tok.commentText(test_ts.src).?);

            const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ "<!--", text, "-->" });
            defer std.testing.allocator.free(expected_slice);

            try std.testing.expectEqualStrings(expected_slice, tok.slice(test_ts.src));
        }

        fn expectErr(test_tt: *TestTokenStream, err: TokenStream.Error) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.err, tok.info);
            try std.testing.expectEqual(err, tok.info.err.code);
        }
        fn expectNull(test_tt: *TestTokenStream) !void {
            try std.testing.expectEqual(@as(?Tok, null), test_tt.next());
        }
    };
};

test {
    var ts = tests.TestTokenStream{};

    ts.reset("<!-- foo -->").unwrap() catch unreachable;
    try ts.expectComment(" foo ");
    try ts.expectNull();
}
