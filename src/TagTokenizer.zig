//! Struct which can be default initialized, but must
//! have its `reset` or `resetUnchecked` function called on it before
//! being able to produce valid results with its `next` function.
//! Purpose is to tokenize XML tags. That is, anything beginning with "<".

const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const math = std.math;
const debug = std.debug;
const testing = std.testing;
const unicode = std.unicode;

const print = debug.print;
const assert = debug.assert;

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");

const TagTokenizer = @This();
frame: @Frame(TagTokenizer.tokenize) = undefined,
tok: *?Tok = undefined,

pub const Error = error{
    UnexpectedEof,
    InvalidCharacter,
    InvalidDoubleDashInComment,
};

pub fn next(tt: *TagTokenizer) ?Tok {
    var tok: ?Tok = null;

    tt.tok = &tok;
    defer tt.tok = undefined;

    resume tt.frame;
    return tok;
}

pub fn reset(tt: *TagTokenizer, src: []const u8) validate_slice.ValidateSliceResult {
    const validation_result = validate_slice.validateUtf8Slice(src);
    switch (validation_result) {
        .ok => tt.resetUnchecked(src),
        .err => {},
    }
    return validation_result;
}

pub fn resetUnchecked(tt: *TagTokenizer, src: []const u8) void {
    tt.* = .{};
    tt.frame = async tt.tokenize(src);
}

pub const Tok = struct {
    index: usize,
    info: Info,

    pub fn init(index: usize, comptime id: Tok.Id, expr: anytype) Tok {
        return Tok{
            .index = index,
            .info = @unionInit(Info, @tagName(id), expr),
        };
    }

    pub const Id = meta.Tag(Info);
    pub const Info = union(enum) {
        /// indicates an error in the source
        err: Err,

        /// indicates '<?'
        pi_start,
        /// indicates the name following 'pi_start'
        pi_target: Len,
        /// indicates a non-string token following 'pi_target', 'pi_str', or 'pi_tok'
        pi_tok: Len,
        /// indicates a string following 'pi_target', 'pi_tok', or 'pi_str'
        pi_str: Len,
        /// indicates '?>'
        pi_end,

        /// indicates '<!--'
        comment_start,
        /// indicates the text following 'comment_start'
        comment_text: Len,
        /// indicates '-->'
        comment_end,

        /// indicates '<![CDATA['
        cdata_start,
        /// indicates the text following 'cdata_start'
        cdata_text: Len,
        /// indicates ']]>'
        cdata_end,

        /// indicates '<'
        elem_open_start,
        /// indicates '</'
        elem_close_start,
        /// indicates '/>'
        elem_close_inline,
        /// indicates '>'
        elem_tag_end,
        /// indicates the name following 'elem_open_start' or 'elem_close_start'
        elem_tag_name: Len,

        /// indicates the name of an attribute
        attr_name: Len,
        /// indicates '='
        attr_eql,
        /// indicates '\"' or '\''
        attr_quote,
        /// indicates text following 'attr_quote' or 'attr_val_entref'
        attr_val_text: Len,

        /// indicates '&' within an attribute value
        attr_val_entref_start,
        /// indicates the id following 'attr_val_entref_start'
        attr_val_entref_id: Len,
        /// indicates ';'
        attr_val_entref_end,

        pub const Err = struct { code: Error };
        pub const Len = struct { len: usize };
    };

    pub fn slice(tok: Tok, src: []const u8) []const u8 {
        const start = tok.index;
        return switch (tok.info) {
            .err => unreachable,
            .comment_start => src[start .. start + "<!--".len],
            .comment_text => |info| src[start .. start + info.len],
            .comment_end => src[start .. start + "-->".len],
            .cdata_start => src[start .. start + "<![CDATA[".len],
            .cdata_text => |info| src[start .. start + info.len],
            .cdata_end => src[start .. start + "]]>".len],
            .pi_start => src[start .. start + "<?".len],
            .pi_target => |info| src[start .. start + info.len],
            .pi_tok => |info| src[start .. start + info.len],
            .pi_str => |info| src[start .. start + info.len],
            .pi_end => src[start .. start + "?>".len],
            .elem_open_start => src[start .. start + "<".len],
            .elem_close_start => src[start .. start + "</".len],
            .elem_close_inline => src[start .. start + "/>".len],
            .elem_tag_end => src[start .. start + ">".len],
            .elem_tag_name => |info| src[start .. start + info.len],
            .attr_name => |info| src[start .. start + info.len],
            .attr_eql => src[start .. start + "=".len],
            .attr_quote => src[start .. start + "'".len],
            .attr_val_text => |info| src[start .. start + info.len],
            .attr_val_entref_start => src[start .. start + "&".len],
            .attr_val_entref_id => |info| src[start .. start + info.len],
            .attr_val_entref_end => src[start .. start + ";".len],
        };
    }

    pub fn expectedSlice(tok: Tok) ?[]const u8 {
        return switch (tok.info) {
            .err => null,
            .comment_start => "<!--",
            .comment_text => null,
            .comment_end => "-->",
            .cdata_start => "<![CDATA[",
            .cdata_text => null,
            .cdata_end => "]]>",
            .pi_start => "<?",
            .pi_target => null,
            .pi_tok => null,
            .pi_str => null,
            .pi_end => "?>",
            .elem_open_start => "<",
            .elem_close_start => "</",
            .elem_close_inline => "/>",
            .elem_tag_end => ">",
            .elem_tag_name => null,
            .attr_name => null,
            .attr_eql => "=",
            .attr_quote => null,
            .attr_val_text => null,
            .attr_val_entref_start => "&",
            .attr_val_entref_id => null,
            .attr_val_entref_end => ";",
        };
    }
};

fn tokenize(tt: *TagTokenizer, src: []const u8) void {
    var i: usize = 0;
    suspend {}

    tokenization: {
        assert(src.len != 0);
        assert(src[0] == '<');

        i += 1;
        if (i == src.len) {
            suspend tt.setResult(0, .elem_open_start, {});
            tt.setError(i, Error.UnexpectedEof);
            break :tokenization;
        }

        switch (src[i]) {
            '?' => {
                suspend tt.setResult(0, .pi_start, {});

                i += 1;
                if (i == src.len) {
                    tt.setError(i, Error.UnexpectedEof);
                    break :tokenization;
                }

                tokenize_pi_target: {
                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.setError(i, Error.InvalidCharacter);
                        break :tokenization;
                    };
                    i = utility.nextNonXmlNameCharIndexAfter(src, i);

                    suspend tt.setResult("<?".len, .pi_target, .{ .len = i - "<?".len });
                    break :tokenize_pi_target;
                }

                if (i == src.len) {
                    tt.setError(i, Error.UnexpectedEof);
                    break :tokenization;
                }
                if (mem.startsWith(u8, src[i..], "?>")) {
                    tt.setResult(i, .pi_end, {});
                    break :tokenization;
                }
                if (!utility.isXmlWhitespaceChar(src[i])) {
                    tt.setError(i, Error.InvalidCharacter);
                    break :tokenization;
                }

                get_tokens: while (true) {
                    i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                    if (i == src.len) {
                        tt.setError(i, Error.UnexpectedEof);
                        break :tokenization;
                    }

                    if (mem.startsWith(u8, src[i..], "?>")) {
                        tt.setResult(i, .pi_end, {});
                        break :tokenization;
                    }
                    switch (src[i]) {
                        '\'', '\"' => {
                            const pi_str_start_index = i;
                            i += 1;

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote = @intToEnum(QuoteType, src[pi_str_start_index]);

                            while (i < src.len) : (i += unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {
                                if (src[i] == @enumToInt(quote)) {
                                    i += 1;
                                    suspend tt.setResult(pi_str_start_index, .pi_str, .{ .len = i - pi_str_start_index });
                                    continue :get_tokens;
                                }
                            }
                        },
                        else => {
                            const pi_tok_start_index = i;
                            while (i < src.len) : (i += unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {
                                if (utility.isXmlWhitespaceChar(src[i]) or mem.startsWith(u8, src[i..], "?>")) {
                                    suspend tt.setResult(pi_tok_start_index, .pi_tok, .{ .len = i - pi_tok_start_index });
                                    continue :get_tokens;
                                }
                            }
                        },
                    }
                }
            },
            '!' => {
                i += 1;
                if (i == src.len) {
                    tt.setError(i, Error.UnexpectedEof);
                    break :tokenization;
                }

                switch (src[i]) {
                    '-' => {
                        i += 1;
                        if (i == src.len) {
                            tt.setError(i, Error.UnexpectedEof);
                            break :tokenization;
                        }

                        switch (src[i]) {
                            '-' => {
                                suspend tt.setResult(0, .comment_start, {});

                                i += 1;
                                if (i == src.len) {
                                    tt.setError(i, Error.UnexpectedEof);
                                    break :tokenization;
                                }
                                if (mem.startsWith(u8, src[i..], "--")) {
                                    if (i + "--".len >= src.len or src[i + "--".len] != '>')
                                        tt.setError(i - "--".len, Error.InvalidDoubleDashInComment)
                                    else
                                        tt.setResult("<!--".len, .comment_end, {});
                                    break :tokenization;
                                }

                                seek_double_slash: while (true) {
                                    i += unicode.utf8ByteSequenceLength(src[i]) catch unreachable;

                                    if (i == src.len or mem.startsWith(u8, src[i..], "--")) {
                                        suspend tt.setResult("<!--".len, .comment_text, .{ .len = i - "<!--".len });

                                        if (i == src.len) {
                                            tt.setError(i, Error.UnexpectedEof);
                                            break :tokenization;
                                        }
                                        break :seek_double_slash;
                                    }
                                }

                                assert(mem.startsWith(u8, src[i..], "--"));
                                i += "--".len;

                                if (i == src.len or src[i] != '>') {
                                    tt.setError(i - "--".len, Error.InvalidDoubleDashInComment);
                                    break :tokenization;
                                }

                                tt.setResult(i - "--".len, .comment_end, {});
                                break :tokenization;
                            },
                            else => {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            },
                        }
                    },
                    '[' => {
                        inline for ("CDATA[") |expected_char| {
                            i += 1;
                            if (i == src.len) {
                                tt.setError(i, Error.UnexpectedEof);
                                break :tokenization;
                            }

                            if (src[i] != expected_char) {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }
                        }
                        suspend tt.setResult(0, .cdata_start, {});

                        i += 1;
                        if (i == src.len) {
                            tt.setError(i, Error.UnexpectedEof);
                            break :tokenization;
                        }

                        if (mem.startsWith(u8, src[i..], "]]>")) {
                            tt.setResult("<![CDATA[".len, .cdata_end, {});
                            break :tokenization;
                        }

                        while (true) {
                            i += unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
                            if (i == src.len or mem.startsWith(u8, src[i..], "]]>")) {
                                suspend tt.setResult("<![CDATA[".len, .cdata_text, .{ .len = i - "<![CDATA[".len });

                                if (i == src.len) {
                                    tt.setError(i, Error.UnexpectedEof);
                                    break :tokenization;
                                }
                                break;
                            }
                        }

                        assert(mem.startsWith(u8, src[i..], "]]>"));
                        tt.setResult(i, .cdata_end, {});
                        break :tokenization;
                    },
                    else => {
                        tt.setError(i, Error.InvalidCharacter);
                        break :tokenization;
                    },
                }
            },
            '/' => {
                suspend tt.setResult(0, .elem_close_start, {});

                tokenize_name: {
                    i += 1;
                    if (i == src.len) {
                        tt.setError(i, Error.UnexpectedEof);
                        break :tokenization;
                    }

                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.setError(i, Error.InvalidCharacter);
                        break :tokenization;
                    };
                    i = utility.nextNonXmlNameCharIndexAfter(src, i);

                    suspend tt.setResult("</".len, .elem_tag_name, .{ .len = i - "</".len });
                    break :tokenize_name;
                }
                i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);

                if (i == src.len) {
                    tt.setError(i, Error.UnexpectedEof);
                    break :tokenization;
                }
                if (src[i] != '>') {
                    tt.setError(i, Error.InvalidCharacter);
                    break :tokenization;
                }

                tt.setResult(i, .elem_tag_end, {});
                break :tokenization;
            },
            else => {
                suspend tt.setResult(0, .elem_open_start, {});

                tokenize_name: {
                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.setError(i, Error.InvalidCharacter);
                        break :tokenization;
                    };
                    i = utility.nextNonXmlNameCharIndexAfter(src, i);

                    suspend tt.setResult("<".len, .elem_tag_name, .{ .len = i - "<".len });
                    break :tokenize_name;
                }

                get_attributes: while (true) {
                    i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                    if (i == src.len) {
                        tt.setError(i, Error.UnexpectedEof);
                        break :tokenization;
                    }

                    switch (src[i]) {
                        '>' => {
                            tt.setResult(i, .elem_tag_end, {});
                            break :tokenization;
                        },
                        '/' => {
                            if (i + 1 == src.len) {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }

                            if (src[i + 1] != '>') {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }

                            tt.setResult(i, .elem_close_inline, {});
                            break :tokenization;
                        },
                        else => {
                            tokenize_attr_name: {
                                const attr_name_start_index = i;
                                i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                                    tt.setError(i, Error.InvalidCharacter);
                                    break :tokenization;
                                };
                                i = utility.nextNonXmlNameCharIndexAfter(src, i);

                                suspend tt.setResult(attr_name_start_index, .attr_name, .{ .len = i - attr_name_start_index });
                                break :tokenize_attr_name;
                            }

                            i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                            if (i == src.len) {
                                tt.setError(i, Error.UnexpectedEof);
                                break :tokenization;
                            }

                            if (src[i] != '=') {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }

                            suspend tt.setResult(i, .attr_eql, {});

                            i += 1;
                            i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                            if (i == src.len) {
                                tt.setError(i, Error.UnexpectedEof);
                                break :tokenization;
                            }

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote: QuoteType = switch (src[i]) {
                                '\"', '\'' => @intToEnum(QuoteType, src[i]),
                                else => {
                                    tt.setError(i, Error.InvalidCharacter);
                                    break :tokenization;
                                },
                            };

                            suspend tt.setResult(i, .attr_quote, {});

                            i += 1;

                            get_attr_value: while (true) {
                                if (i == src.len) {
                                    tt.setError(i, Error.UnexpectedEof);
                                    break :tokenization;
                                }

                                if (@enumToInt(quote) == src[i]) {
                                    suspend tt.setResult(i, .attr_quote, {});

                                    i += 1;
                                    if (i == src.len) {
                                        tt.setError(i, Error.UnexpectedEof);
                                        break :tokenization;
                                    }

                                    if (src[i] != '>' and
                                        src[i] != '/' and
                                        !utility.isXmlWhitespaceChar(src[i]))
                                    {
                                        tt.setError(i, Error.InvalidCharacter);
                                        break :tokenization;
                                    }

                                    continue :get_attributes;
                                }

                                if (src[i] == '&') {
                                    suspend tt.setResult(i, .attr_val_entref_start, {});

                                    const attr_entref_name_start_index = i;
                                    _ = attr_entref_name_start_index;

                                    i += 1;
                                    if (i == src.len) {
                                        tt.setError(i, Error.UnexpectedEof);
                                        break :tokenization;
                                    }
                                    switch (src[i]) {
                                        '#' => {
                                            const entref_id_start_index = i;

                                            i += 1;
                                            if (i == src.len) {
                                                tt.setError(i, Error.UnexpectedEof);
                                                break :tokenization;
                                            }

                                            if (mem.startsWith(u8, src[i..], "0x")) i += "0x".len;
                                            if (i == src.len) {
                                                tt.setError(i, Error.UnexpectedEof);
                                                break :tokenization;
                                            }

                                            switch (src[i]) {
                                                '0'...'9',
                                                'a'...'f',
                                                'A'...'F',
                                                => {},
                                                else => {
                                                    tt.setError(i, Error.InvalidCharacter);
                                                    break :tokenization;
                                                },
                                            }

                                            while (i < src.len) switch (src[i]) {
                                                '0'...'9',
                                                'a'...'f',
                                                'A'...'F',
                                                => i += 1,
                                                else => break,
                                            };

                                            suspend tt.setResult(entref_id_start_index, .attr_val_entref_id, .{ .len = i - entref_id_start_index });
                                        },
                                        else => {
                                            const entref_id_start_index = i;
                                            i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                                                tt.setError(i, Error.InvalidCharacter);
                                                break :tokenization;
                                            };
                                            i = utility.nextNonXmlNameCharIndexAfter(src, i);

                                            suspend tt.setResult(entref_id_start_index, .attr_val_entref_id, .{ .len = i - entref_id_start_index });
                                        },
                                    }

                                    if (i == src.len) {
                                        tt.setError(i, Error.UnexpectedEof);
                                        break :tokenization;
                                    }
                                    if (src[i] != ';') {
                                        tt.setError(i, Error.InvalidCharacter);
                                        break :tokenization;
                                    }

                                    suspend tt.setResult(i, .attr_val_entref_end, {});

                                    i += 1;
                                    continue :get_attr_value;
                                }

                                const text_value_start_index = i;
                                while (i < src.len and src[i] != @enumToInt(quote) and src[i] != '&') : (i += unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {}

                                suspend tt.setResult(text_value_start_index, .attr_val_text, .{ .len = i - text_value_start_index });
                                continue :get_attr_value;
                            }
                            unreachable;
                        },
                    }
                    unreachable;
                }
                unreachable;
            },
        }
        unreachable;
    }

    assert(tt.tok.* != null);
    while (true) {
        suspend {}
    }
}

fn setResult(tt: *TagTokenizer, index: usize, comptime id: Tok.Id, expr: anytype) void {
    assert(tt.tok.* == null);
    tt.tok.* = Tok.init(index, id, expr);
}

fn setError(tt: *TagTokenizer, index: usize, code: Error) void {
    tt.setResult(index, .err, .{ .code = code });
}

const tests = struct {
    const TestTagTokenizer = struct {
        tt: TagTokenizer = .{},
        src: []const u8 = &.{},

        fn reset(test_tt: *TestTagTokenizer, src: []const u8) validate_slice.ValidateSliceResult {
            test_tt.src = src;
            return test_tt.tt.reset(test_tt.src);
        }

        fn resetUnchecked(test_tt: *TestTagTokenizer, src: []const u8) void {
            test_tt.src = src;
            test_tt.tt.resetUnchecked(test_tt.src);
        }

        fn next(test_tt: *TestTagTokenizer) ?Tok {
            return test_tt.tt.next();
        }
    };

    fn expectPiStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectPiTarget(test_tt: *TestTagTokenizer, name: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_target, tok.info);
        try testing.expectEqualStrings(name, tok.slice(test_tt.src));
    }
    fn expectPiTok(test_tt: *TestTagTokenizer, token: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_tok, tok.info);
        try testing.expectEqualStrings(token, tok.slice(test_tt.src));
    }
    fn expectPiStr(test_tt: *TestTagTokenizer, str: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_str, tok.info);
        try testing.expectEqualStrings(str, tok.slice(test_tt.src));
    }
    fn expectPiEnd(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }

    fn expectCommentStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectCommentText(test_tt: *TestTagTokenizer, text: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(test_tt.src));
    }
    fn expectCommentEnd(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }

    fn expectCDataStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectCDataText(test_tt: *TestTagTokenizer, text: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(test_tt.src));
    }
    fn expectCDataEnd(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }

    fn expectElemOpenStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_open_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectElemCloseStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_close_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectElemCloseInline(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_close_inline, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectElemTagEnd(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_tag_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectElemTagName(test_tt: *TestTagTokenizer, name: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_tag_name, tok.info);
        try testing.expectEqualStrings(name, tok.slice(test_tt.src));
    }

    fn expectAttrName(test_tt: *TestTagTokenizer, name: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_name, tok.info);
        try testing.expectEqualStrings(name, tok.slice(test_tt.src));
    }
    fn expectAttrEql(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_eql, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectAttrQuote(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_quote, tok.info);
        try testing.expectEqual(@as(usize, 1), tok.slice(test_tt.src).len);
        const actual = tok.slice(test_tt.src)[0];
        try testing.expect(actual == '\"' or actual == '\'');
    }
    fn expectAttrValText(test_tt: *TestTagTokenizer, text: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(test_tt.src));
    }
    fn expectAttrValEntrefStart(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }
    fn expectAttrValEntrefId(test_tt: *TestTagTokenizer, id: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_id, tok.info);
        try testing.expectEqualStrings(id, tok.slice(test_tt.src));
    }
    fn expectAttrValEntrefEnd(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(test_tt.src));
    }

    fn expectErr(test_tt: *TestTagTokenizer, err: TagTokenizer.Error) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(TagTokenizer.Tok.Id.err, tok.info);
        try testing.expectEqual(err, tok.info.err.code);
        switch (err) {
            Error.InvalidDoubleDashInComment => try testing.expectEqualStrings("--", test_tt.src[tok.index .. tok.index + "--".len]),
            else => {},
        }
    }
    fn expectNull(test_tt: *TestTagTokenizer) !void {
        try testing.expectEqual(@as(?TagTokenizer.Tok, null), test_tt.next());
    }

    pub usingnamespace shorthands;
    const shorthands = struct {
        pub fn expectAttrValEntref(test_tt: *TestTagTokenizer, id: []const u8) !void {
            try tests.expectAttrValEntrefStart(test_tt);
            try tests.expectAttrValEntrefId(test_tt, id);
            try tests.expectAttrValEntrefEnd(test_tt);
        }

        const AttrVal = union(enum) { text: []const u8, entref: []const u8 };
        pub fn expectAttrVal(test_tt: *TestTagTokenizer, segments: []const AttrVal) !void {
            try tests.expectAttrQuote(test_tt);
            for (segments) |seg| try switch (seg) {
                .text => |text| tests.expectAttrValText(test_tt, text),
                .entref => |id| tests.expectAttrValEntref(test_tt, id),
            };
            try tests.expectAttrQuote(test_tt);
        }

        pub fn expectAttr(test_tt: *TestTagTokenizer, name: []const u8, val: []const AttrVal) !void {
            try tests.expectAttrName(test_tt, name);
            try tests.expectAttrEql(test_tt);
            try tests.expectAttrVal(test_tt, val);
        }
    };
};

test "TagTokenizer Unexpected Eof" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<?").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!").unwrap() catch unreachable;
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!-").unwrap() catch unreachable;
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    inline for ([_]void{undefined} ** "[CDATA".len) |_, i| {
        tt.reset("<!" ++ ("[CDATA"[0 .. i + 1])).unwrap() catch unreachable;
        try tests.expectErr(&tt, Error.UnexpectedEof);
        try tests.expectNull(&tt);
    }
}

test "TagTokenizer PI" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<?a").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<?a?").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<?a?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?a ?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc d?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "d");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = ''?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "def");
    try tests.expectPiTok(&tt, "=");
    try tests.expectPiStr(&tt, "''");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = \"g\"?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "def");
    try tests.expectPiTok(&tt, "=");
    try tests.expectPiStr(&tt, "\"g\"");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = 'ghi'\"\" ?>").unwrap() catch unreachable;
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "def");
    try tests.expectPiTok(&tt, "=");
    try tests.expectPiStr(&tt, "'ghi'");
    try tests.expectPiStr(&tt, "\"\"");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer Comment" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<!--").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!---").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, "-");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!----").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!---->").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<!----->").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!-- --->").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, " ");
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!--- -->").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, "- ");
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<!-- - -->").unwrap() catch unreachable;
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, " - ");
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer CDATA" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<![CDATA[").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "]");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]]").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "]]");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]]>").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectCDataEnd(&tt);
    try tests.expectNull(&tt);

    inline for (.{
        "]",
        "]]",
        "]]]",
        "]]]]",
        "]>",
        "]>]",
    }) |maybe_ambiguous_content| {
        tt.reset("<![CDATA[" ++ maybe_ambiguous_content ++ "]]>").unwrap() catch unreachable;
        try tests.expectCDataStart(&tt);
        try tests.expectCDataText(&tt, maybe_ambiguous_content);
        try tests.expectCDataEnd(&tt);
        try tests.expectNull(&tt);
    }

    tt.reset("<![CDATA[a]]>").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "a");
    try tests.expectCDataEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[ foobar ]]>").unwrap() catch unreachable;
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, " foobar ");
    try tests.expectCDataEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer Element Close" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("</").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</a").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</foo").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</foo ñ").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("</foo>").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("</foo >").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("</foo\t >").unwrap() catch unreachable;
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer Element Open" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<a").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo/").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo /").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo >").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo />").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar ").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo bar />").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo bar = ").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectAttrEql(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo bar = />").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectAttrEql(&tt);
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo bar=''/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar=\"baz\"/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .text = "baz" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&baz;'/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "baz" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&#0123456789abcdef;'/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "#0123456789abcdef" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&#0x0123456789abcdef;'/>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "#0x0123456789abcdef" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='foo&bar;baz'>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");

    try tests.expectAttr(&tt, "B", &.{ .{ .text = "foo" }, .{ .entref = "bar" }, .{ .text = "baz" } });

    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='&foo;bar&baz;'>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");
    try tests.expectAttrName(&tt, "B");
    try tests.expectAttrEql(&tt);
    try tests.expectAttrVal(&tt, &.{ .{ .entref = "foo" }, .{ .text = "bar" }, .{ .entref = "baz" } });
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='&foo;&bar;&baz;'>").unwrap() catch unreachable;
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");
    try tests.expectAttrName(&tt, "B");
    try tests.expectAttrEql(&tt);
    try tests.expectAttrVal(&tt, &.{ .{ .entref = "foo" }, .{ .entref = "bar" }, .{ .entref = "baz" } });
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);
}
