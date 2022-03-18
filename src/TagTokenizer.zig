//! Struct which can be default initialized, but must
//! have its `reset` or `resetUnchecked` function called on it before
//! being able to produce valid results with its `next` function.
//! Purpose is to tokenize XML tags. That is, anything beginning with "<".

const std = @import("std");

const validate_slice = @import("validate_slice.zig");
const utility = @import("utility.zig");

const TagTokenizer = @This();
frame: @Frame(TagTokenizer.tokenize) = undefined,
tok: *?Tok = undefined,

pub const Error = error{
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

        pub fn cannonicalSlice(tag: std.meta.Tag(Info)) ?[]const u8 {
            return switch (tag) {
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

    pub fn slice(tok: Tok, src: []const u8) []const u8 {
        std.debug.assert(tok.info != .err);
        const start = tok.index;
        const end = tok.index + tok.len();
        return src[start..end];
    }

    pub fn len(tok: Tok) usize {
        return switch (tok.info) {
            .err => unreachable,

            .pi_start => "<?".len,
            .pi_target => |pi_target| pi_target.len,
            .pi_tok => |pi_tok| pi_tok.len,
            .pi_str => |pi_str| pi_str.len,
            .pi_end => "?>".len,

            .comment_start => "<!--".len,
            .comment_text => |comment_text| comment_text.len,
            .comment_end => "-->".len,

            .cdata_start => "<![CDATA[".len,
            .cdata_text => |cdata_text| cdata_text.len,
            .cdata_end => "]]>".len,

            .elem_open_start => "<".len,
            .elem_close_start => "</".len,
            .elem_close_inline => "/>".len,
            .elem_tag_end => ">".len,
            .elem_tag_name => |elem_tag_name| elem_tag_name.len,

            .attr_name => |attr_name| attr_name.len,
            .attr_eql => "=".len,
            .attr_quote => "'".len,
            .attr_val_text => |attr_val_text| attr_val_text.len,

            .attr_val_entref_start => "&".len,
            .attr_val_entref_id => |attr_val_entref_id| attr_val_entref_id.len,
            .attr_val_entref_end => ";".len,
        };
    }
};

pub fn fmtTok(tok: Tok, src: []const u8) std.fmt.Formatter(formatTok) {
    return .{ .data = .{ .tok = tok, .src = src } };
}

pub const FormattableTok = struct { tok: Tok, src: []const u8 };
pub fn formatTok(
    value: FormattableTok,
    comptime format: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try std.fmt.formatText(value.tok.slice(value.src), format, options, writer);
}

fn tokenize(tt: *TagTokenizer, src: []const u8) void {
    var i: usize = 0;
    suspend {}

    tokenization: {
        if (i == src.len) break :tokenization;
        if (src[i] != '<') {
            tt.setError(i, Error.InvalidCharacter);
            break :tokenization;
        }

        i += 1;
        if (i == src.len) {
            suspend tt.setResult(0, .elem_open_start, {});

            break :tokenization;
        }

        switch (src[i]) {
            '?' => {
                suspend tt.setResult(0, .pi_start, {});

                i += 1;
                if (i == src.len) break :tokenization;

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
                    break :tokenization;
                }
                if (std.mem.startsWith(u8, src[i..], "?>")) {
                    tt.setResult(i, .pi_end, {});
                    break :tokenization;
                }
                if (!utility.isXmlWhitespaceChar(src[i])) {
                    tt.setError(i, Error.InvalidCharacter);
                    break :tokenization;
                }

                get_tokens: while (true) {
                    i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                    if (i == src.len) break :tokenization;

                    if (std.mem.startsWith(u8, src[i..], "?>")) {
                        tt.setResult(i, .pi_end, {});
                        break :tokenization;
                    }
                    switch (src[i]) {
                        '\'', '\"' => {
                            const pi_str_start_index = i;
                            i += 1;

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote = @intToEnum(QuoteType, src[pi_str_start_index]);

                            while (i < src.len) : (i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {
                                if (src[i] == @enumToInt(quote)) {
                                    i += 1;
                                    suspend tt.setResult(pi_str_start_index, .pi_str, .{ .len = i - pi_str_start_index });
                                    continue :get_tokens;
                                }
                            }
                        },
                        else => {
                            const pi_tok_start_index = i;
                            while (i < src.len) : (i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {
                                if (utility.isXmlWhitespaceChar(src[i]) or std.mem.startsWith(u8, src[i..], "?>")) {
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
                if (i == src.len) break :tokenization;

                switch (src[i]) {
                    '-' => {
                        i += 1;
                        if (i == src.len) break :tokenization;

                        switch (src[i]) {
                            '-' => {
                                suspend tt.setResult(0, .comment_start, {});

                                i += 1;
                                if (i == src.len) break :tokenization;
                                if (std.mem.startsWith(u8, src[i..], "--")) {
                                    if (i + "--".len >= src.len or src[i + "--".len] != '>')
                                        tt.setError(i - "--".len, Error.InvalidDoubleDashInComment)
                                    else
                                        tt.setResult("<!--".len, .comment_end, {});
                                    break :tokenization;
                                }

                                seek_double_slash: while (true) {
                                    i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable;

                                    if (i == src.len or std.mem.startsWith(u8, src[i..], "--")) {
                                        suspend tt.setResult("<!--".len, .comment_text, .{ .len = i - "<!--".len });
                                        if (i == src.len) break :tokenization;
                                        break :seek_double_slash;
                                    }
                                }

                                std.debug.assert(std.mem.startsWith(u8, src[i..], "--"));
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
                            if (i == src.len) break :tokenization;
                            if (src[i] != expected_char) {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }
                        }
                        suspend tt.setResult(0, .cdata_start, {});

                        i += 1;
                        if (i == src.len) break :tokenization;
                        if (std.mem.startsWith(u8, src[i..], "]]>")) {
                            tt.setResult("<![CDATA[".len, .cdata_end, {});
                            break :tokenization;
                        }

                        while (true) {
                            i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
                            if (i == src.len or std.mem.startsWith(u8, src[i..], "]]>")) {
                                suspend tt.setResult("<![CDATA[".len, .cdata_text, .{ .len = i - "<![CDATA[".len });
                                if (i == src.len) break :tokenization;
                                break;
                            }
                        }

                        std.debug.assert(std.mem.startsWith(u8, src[i..], "]]>"));
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
                    if (i == src.len) break :tokenization;

                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.setError(i, Error.InvalidCharacter);
                        break :tokenization;
                    };
                    i = utility.nextNonXmlNameCharIndexAfter(src, i);

                    suspend tt.setResult("</".len, .elem_tag_name, .{ .len = i - "</".len });
                    break :tokenize_name;
                }
                i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);

                if (i == src.len) break :tokenization;
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
                    if (i == src.len) break :tokenization;

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
                            if (i == src.len) break :tokenization;
                            if (src[i] != '=') {
                                tt.setError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }

                            suspend tt.setResult(i, .attr_eql, {});

                            i += 1;
                            i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
                            if (i == src.len) break :tokenization;

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
                                if (i == src.len) break :tokenization;
                                if (@enumToInt(quote) == src[i]) {
                                    suspend tt.setResult(i, .attr_quote, {});

                                    i += 1;
                                    if (i == src.len) break :tokenization;
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
                                    if (i == src.len) break :tokenization;
                                    switch (src[i]) {
                                        '#' => {
                                            const entref_id_start_index = i;

                                            i += 1;
                                            if (i == src.len) break :tokenization;

                                            if (std.mem.startsWith(u8, src[i..], "0x")) i += "0x".len;
                                            if (i == src.len) break :tokenization;

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

                                    if (i == src.len) break :tokenization;
                                    if (src[i] != ';') {
                                        tt.setError(i, Error.InvalidCharacter);
                                        break :tokenization;
                                    }

                                    suspend tt.setResult(i, .attr_val_entref_end, {});

                                    i += 1;
                                    continue :get_attr_value;
                                }

                                const text_value_start_index = i;
                                while (i < src.len and src[i] != @enumToInt(quote) and src[i] != '&') : (i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {}

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

    std.debug.assert(tt.tok.* != null or i == src.len);
    while (true) {
        suspend {}
    }
}

fn setResult(tt: *TagTokenizer, index: usize, comptime id: Tok.Id, expr: anytype) void {
    std.debug.assert(tt.tok.* == null);
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

        fn expectPiStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.pi_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectPiTarget(test_tt: *TestTagTokenizer, name: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.pi_target, tok.info);
            try std.testing.expectEqualStrings(name, tok.slice(test_tt.src));
        }
        fn expectPiTok(test_tt: *TestTagTokenizer, token: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.pi_tok, tok.info);
            try std.testing.expectEqualStrings(token, tok.slice(test_tt.src));
        }
        fn expectPiStr(test_tt: *TestTagTokenizer, str: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.pi_str, tok.info);
            try std.testing.expectEqualStrings(str, tok.slice(test_tt.src));
        }
        fn expectPiEnd(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.pi_end, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }

        fn expectCommentStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.comment_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectCommentText(test_tt: *TestTagTokenizer, text: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.comment_text, tok.info);
            try std.testing.expectEqualStrings(text, tok.slice(test_tt.src));
        }
        fn expectCommentEnd(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.comment_end, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }

        fn expectCDataStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.cdata_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectCDataText(test_tt: *TestTagTokenizer, text: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.cdata_text, tok.info);
            try std.testing.expectEqualStrings(text, tok.slice(test_tt.src));
        }
        fn expectCDataEnd(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.cdata_end, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }

        fn expectElemOpenStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.elem_open_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectElemCloseStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.elem_close_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectElemCloseInline(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.elem_close_inline, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectElemTagEnd(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.elem_tag_end, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectElemTagName(test_tt: *TestTagTokenizer, name: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.elem_tag_name, tok.info);
            try std.testing.expectEqualStrings(name, tok.slice(test_tt.src));
        }

        fn expectAttrName(test_tt: *TestTagTokenizer, name: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_name, tok.info);
            try std.testing.expectEqualStrings(name, tok.slice(test_tt.src));
        }
        fn expectAttrEql(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_eql, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectAttrQuote(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_quote, tok.info);
            try std.testing.expectEqual(@as(usize, 1), tok.slice(test_tt.src).len);
            const actual = tok.slice(test_tt.src)[0];
            try std.testing.expect(actual == '\"' or actual == '\'');
        }
        fn expectAttrValText(test_tt: *TestTagTokenizer, text: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_val_text, tok.info);
            try std.testing.expectEqualStrings(text, tok.slice(test_tt.src));
        }
        fn expectAttrValEntrefStart(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_val_entref_start, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }
        fn expectAttrValEntrefId(test_tt: *TestTagTokenizer, id: []const u8) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_val_entref_id, tok.info);
            try std.testing.expectEqualStrings(id, tok.slice(test_tt.src));
        }
        fn expectAttrValEntrefEnd(test_tt: *TestTagTokenizer) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(Tok.Id.attr_val_entref_end, tok.info);
            try std.testing.expectEqualStrings(tok.info.cannonicalSlice().?, tok.slice(test_tt.src));
        }

        fn expectErr(test_tt: *TestTagTokenizer, err: TagTokenizer.Error) !void {
            const tok = test_tt.next() orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(TagTokenizer.Tok.Id.err, tok.info);
            try std.testing.expectEqual(err, tok.info.err.code);
            switch (err) {
                Error.InvalidDoubleDashInComment => try std.testing.expectEqualStrings("--", test_tt.src[tok.index .. tok.index + "--".len]),
                else => {},
            }
        }
        fn expectNull(test_tt: *TestTagTokenizer) !void {
            try std.testing.expectEqual(@as(?TagTokenizer.Tok, null), test_tt.next());
        }

        pub usingnamespace shorthands;
        const shorthands = struct {
            pub fn expectAttrValEntref(test_tt: *TestTagTokenizer, id: []const u8) !void {
                try test_tt.expectAttrValEntrefStart();
                try test_tt.expectAttrValEntrefId(id);
                try test_tt.expectAttrValEntrefEnd();
            }

            const AttrVal = union(enum) { text: []const u8, entref: []const u8 };
            pub fn expectAttrVal(test_tt: *TestTagTokenizer, segments: []const AttrVal) !void {
                try test_tt.expectAttrQuote();
                for (segments) |seg| try switch (seg) {
                    .text => |text| test_tt.expectAttrValText(text),
                    .entref => |id| test_tt.expectAttrValEntref(id),
                };
                try test_tt.expectAttrQuote();
            }

            pub fn expectAttr(test_tt: *TestTagTokenizer, name: []const u8, val: []const AttrVal) !void {
                try test_tt.expectAttrName(name);
                try test_tt.expectAttrEql();
                try test_tt.expectAttrVal(val);
            }
        };
    };
};

test "Tok Format" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<?abc d?>").unwrap() catch unreachable;
    // zig fmt: off
    try std.testing.expectFmt("<?",  "{s}", .{fmtTok(tt.next().?, tt.src)});
    try std.testing.expectFmt("abc", "{s}", .{fmtTok(tt.next().?, tt.src)});
    try std.testing.expectFmt("d",   "{s}", .{fmtTok(tt.next().?, tt.src)});
    try std.testing.expectFmt("?>",  "{s}", .{fmtTok(tt.next().?, tt.src)});
    try tt.expectNull();
    // zig fmt: on
}

test "TagTokenizer Empty" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("").unwrap() catch unreachable;
    try tt.expectNull();
}

test "TagTokenizer Non-Starter" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("a").unwrap() catch unreachable;
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();
}

test "TagTokenizer Unexpected Eof" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<?").unwrap() catch unreachable;
    try tt.expectPiStart();

    try tt.expectNull();

    tt.reset("<!").unwrap() catch unreachable;

    try tt.expectNull();

    tt.reset("<!-").unwrap() catch unreachable;

    try tt.expectNull();

    inline for ([_]void{undefined} ** "[CDATA".len) |_, i| {
        tt.reset("<!" ++ ("[CDATA"[0 .. i + 1])).unwrap() catch unreachable;

        try tt.expectNull();
    }
}

test "TagTokenizer PI" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<?a").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("a");

    try tt.expectNull();

    tt.reset("<?a?").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("a");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<?a?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("a");
    try tt.expectPiEnd();
    try tt.expectNull();

    tt.reset("<?a ?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("a");
    try tt.expectPiEnd();
    try tt.expectNull();

    tt.reset("<?abc d?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("abc");
    try tt.expectPiTok("d");
    try tt.expectPiEnd();
    try tt.expectNull();

    tt.reset("<?abc def = ''?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("abc");
    try tt.expectPiTok("def");
    try tt.expectPiTok("=");
    try tt.expectPiStr("''");
    try tt.expectPiEnd();
    try tt.expectNull();

    tt.reset("<?abc def = \"g\"?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("abc");
    try tt.expectPiTok("def");
    try tt.expectPiTok("=");
    try tt.expectPiStr("\"g\"");
    try tt.expectPiEnd();
    try tt.expectNull();

    tt.reset("<?abc def = 'ghi'\"\" ?>").unwrap() catch unreachable;
    try tt.expectPiStart();
    try tt.expectPiTarget("abc");
    try tt.expectPiTok("def");
    try tt.expectPiTok("=");
    try tt.expectPiStr("'ghi'");
    try tt.expectPiStr("\"\"");
    try tt.expectPiEnd();
    try tt.expectNull();
}

test "TagTokenizer Comment" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<!--").unwrap() catch unreachable;
    try tt.expectCommentStart();

    try tt.expectNull();

    tt.reset("<!---").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectCommentText("-");

    try tt.expectNull();

    tt.reset("<!----").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectErr(Error.InvalidDoubleDashInComment);
    try tt.expectNull();

    tt.reset("<!---->").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectCommentEnd();
    try tt.expectNull();

    tt.reset("<!----->").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectErr(Error.InvalidDoubleDashInComment);
    try tt.expectNull();

    tt.reset("<!-- --->").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectCommentText(" ");
    try tt.expectErr(Error.InvalidDoubleDashInComment);
    try tt.expectNull();

    tt.reset("<!--- -->").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectCommentText("- ");
    try tt.expectCommentEnd();
    try tt.expectNull();

    tt.reset("<!-- - -->").unwrap() catch unreachable;
    try tt.expectCommentStart();
    try tt.expectCommentText(" - ");
    try tt.expectCommentEnd();
    try tt.expectNull();
}

test "TagTokenizer CDATA" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<![CDATA[").unwrap() catch unreachable;
    try tt.expectCDataStart();

    try tt.expectNull();

    tt.reset("<![CDATA[]").unwrap() catch unreachable;
    try tt.expectCDataStart();
    try tt.expectCDataText("]");

    try tt.expectNull();

    tt.reset("<![CDATA[]]").unwrap() catch unreachable;
    try tt.expectCDataStart();
    try tt.expectCDataText("]]");

    try tt.expectNull();

    tt.reset("<![CDATA[]]>").unwrap() catch unreachable;
    try tt.expectCDataStart();
    try tt.expectCDataEnd();
    try tt.expectNull();

    inline for (.{
        "]",
        "]]",
        "]]]",
        "]]]]",
        "]>",
        "]>]",
    }) |maybe_ambiguous_content| {
        tt.reset("<![CDATA[" ++ maybe_ambiguous_content ++ "]]>").unwrap() catch unreachable;
        try tt.expectCDataStart();
        try tt.expectCDataText(maybe_ambiguous_content);
        try tt.expectCDataEnd();
        try tt.expectNull();
    }

    tt.reset("<![CDATA[a]]>").unwrap() catch unreachable;
    try tt.expectCDataStart();
    try tt.expectCDataText("a");
    try tt.expectCDataEnd();
    try tt.expectNull();

    tt.reset("<![CDATA[ foobar ]]>").unwrap() catch unreachable;
    try tt.expectCDataStart();
    try tt.expectCDataText(" foobar ");
    try tt.expectCDataEnd();
    try tt.expectNull();
}

test "TagTokenizer Element Close" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("</").unwrap() catch unreachable;
    try tt.expectElemCloseStart();

    try tt.expectNull();

    tt.reset("</a").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("a");

    try tt.expectNull();

    tt.reset("</foo").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("foo");

    try tt.expectNull();

    tt.reset("</foo ñ").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("</foo>").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("</foo >").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("</foo\t >").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemTagEnd();
    try tt.expectNull();
}

test "TagTokenizer Element Open" {
    var tt = tests.TestTagTokenizer{};

    tt.reset("<").unwrap() catch unreachable;
    try tt.expectElemOpenStart();

    try tt.expectNull();

    tt.reset("<a").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("a");

    try tt.expectNull();

    tt.reset("<foo").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");

    try tt.expectNull();

    tt.reset("<foo/").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<foo /").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<foo>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("<foo >").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("<foo/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo />").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar ").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");

    try tt.expectNull();

    tt.reset("<foo bar />").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<foo bar = ").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();

    try tt.expectNull();

    tt.reset("<foo bar = />").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<foo bar=''/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar=\"baz\"/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{.{ .text = "baz" }});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar='&baz;'/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{.{ .entref = "baz" }});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar='&#0123456789abcdef;'/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{.{ .entref = "#0123456789abcdef" }});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar='&#0x0123456789abcdef;'/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{.{ .entref = "#0x0123456789abcdef" }});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<A B='foo&bar;baz'>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("A");

    try tt.expectAttr("B", &.{ .{ .text = "foo" }, .{ .entref = "bar" }, .{ .text = "baz" } });

    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("<A B='&foo;bar&baz;'>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("A");
    try tt.expectAttrName("B");
    try tt.expectAttrEql();
    try tt.expectAttrVal(&.{ .{ .entref = "foo" }, .{ .text = "bar" }, .{ .entref = "baz" } });
    try tt.expectElemTagEnd();
    try tt.expectNull();

    tt.reset("<A B='&foo;&bar;&baz;'>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("A");
    try tt.expectAttrName("B");
    try tt.expectAttrEql();
    try tt.expectAttrVal(&.{ .{ .entref = "foo" }, .{ .entref = "bar" }, .{ .entref = "baz" } });
    try tt.expectElemTagEnd();
    try tt.expectNull();
}
