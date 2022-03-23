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
    ExpectedLeftAngleBracket,
    InvalidPiTargetStartChar,
    ExpectedWhitespaceAfterPiTarget,
    IncompleteBangToken,
    IncompleteBangDashToken,
    IncompleteCDataKeyword,
    ExpectedElemCloseNameStartChar,
    UnexpectedCharacterFollowingElemCloseName,
    ExpectedElemOpenNameStartChar,
    IncompleteElemCloseInlineTok,
    ExpectedRightAngleBracket,
    InvalidCharacter,
    ExpectedAttrEql,
    ExpectedAttrQuote,
    InvalidEntrefIdChar,
    InvalidEntrefIdNameChar,
    ExpectedEntrefSemicolon,
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
        pi_open,
        /// indicates the name following 'pi_open'
        pi_target: Len,
        /// indicates a non-string token following 'pi_target', 'pi_str', or 'pi_tok'
        pi_tok: Len,
        /// indicates a string following 'pi_target', 'pi_tok', or 'pi_str'
        pi_str: Len,
        /// indicates '?>'
        pi_close,

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
        /// indicates '\''
        attr_quote_single,
        /// indicates '\"'
        attr_quote_double,
        /// indicates text following 'attr_quote_single', 'attr_quote_double' or 'attr_val_entref'
        attr_val_text: Len,

        /// indicates '&' within an attribute value
        attr_val_entref_start,
        /// indicates the id following 'attr_val_entref_start'
        attr_val_entref_id: Len,
        /// indicates ';'
        attr_val_entref_end,

        pub const Err = struct { code: Error };
        pub const Len = struct { len: usize };

        pub fn cannonicalSlice(tag: Tok.Id) ?[]const u8 {
            return switch (tag) {
                // zig fmt: off
                .err                   => null,

                .comment_start         => "<!--",
                .comment_text          => null,
                .comment_end           => "-->",

                .cdata_start           => "<![CDATA[",
                .cdata_text            => null,
                .cdata_end             => "]]>",

                .pi_open               => "<?",
                .pi_target             => null,
                .pi_tok                => null,
                .pi_str                => null,
                .pi_close              => "?>",

                .elem_open_start       => "<",
                .elem_close_start      => "</",
                .elem_close_inline     => "/>",
                .elem_tag_end          => ">",
                .elem_tag_name         => null,

                .attr_name             => null,
                .attr_eql              => "=",
                .attr_quote_single     => "\'",
                .attr_quote_double     => "\"",
                .attr_val_text         => null,

                .attr_val_entref_start => "&",
                .attr_val_entref_id    => null,
                .attr_val_entref_end   => ";",
                // zig fmt: on
            };
        }

        pub fn isAttrQuote(self: Tok.Id) bool {
            return switch (self) {
                .attr_quote_single,
                .attr_quote_double,
                => true,
                else => false,
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
        const cannonical_len: ?usize = if (tok.info.cannonicalSlice()) |cannonical| cannonical.len else null;
        return switch (tok.info) {
            // zig fmt: off
            .err                   => unreachable,

            .pi_open               => cannonical_len.?,
            .pi_target             => |pi_target| pi_target.len,
            .pi_tok                => |pi_tok| pi_tok.len,
            .pi_str                => |pi_str| pi_str.len,
            .pi_close              => cannonical_len.?,

            .comment_start         => cannonical_len.?,
            .comment_text          => |comment_text| comment_text.len,
            .comment_end           => cannonical_len.?,

            .cdata_start           => cannonical_len.?,
            .cdata_text            => |cdata_text| cdata_text.len,
            .cdata_end             => cannonical_len.?,

            .elem_open_start       => cannonical_len.?,
            .elem_close_start      => cannonical_len.?,
            .elem_close_inline     => cannonical_len.?,
            .elem_tag_end          => cannonical_len.?,
            .elem_tag_name         => |elem_tag_name| elem_tag_name.len,

            .attr_name             => |attr_name| attr_name.len,
            .attr_eql              => cannonical_len.?,
            .attr_quote_single     => cannonical_len.?,
            .attr_quote_double     => cannonical_len.?,
            .attr_val_text         => |attr_val_text| attr_val_text.len,

            .attr_val_entref_start => cannonical_len.?,
            .attr_val_entref_id    => |attr_val_entref_id| attr_val_entref_id.len,
            .attr_val_entref_end   => cannonical_len.?,
            // zig fmt: on
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
            tt.emitError(i, Error.ExpectedLeftAngleBracket);
            break :tokenization;
        }

        i += 1;
        if (i == src.len) {
            suspend tt.emitResult(0, .elem_open_start, {});
            break :tokenization;
        }

        switch (src[i]) {
            '?' => {
                suspend tt.emitResult(0, .pi_open, {});

                i += 1;
                if (i == src.len) break :tokenization;

                tokenize_pi_target: {
                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.emitError(i, Error.InvalidPiTargetStartChar);
                        break :tokenization;
                    };
                    i = utility.xmlNextNonNameCharIndexAfter(src, i);

                    suspend tt.emitResult("<?".len, .pi_target, .{ .len = i - "<?".len });
                    break :tokenize_pi_target;
                }

                if (i == src.len) {
                    break :tokenization;
                }
                if (std.mem.startsWith(u8, src[i..], "?>")) {
                    tt.emitResult(i, .pi_close, {});
                    break :tokenization;
                }
                if (!utility.xmlIsWhitespaceChar(src[i])) {
                    tt.emitError(i, Error.ExpectedWhitespaceAfterPiTarget);
                    break :tokenization;
                }

                get_tokens: while (true) {
                    i = utility.xmlNextNonWhitespaceCharIndexAfter(src, i);
                    if (i == src.len) break :tokenization;

                    if (std.mem.startsWith(u8, src[i..], "?>")) {
                        tt.emitResult(i, .pi_close, {});
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
                                    suspend tt.emitResult(pi_str_start_index, .pi_str, .{ .len = i - pi_str_start_index });
                                    continue :get_tokens;
                                }
                            }
                        },
                        else => {
                            const pi_tok_start_index = i;
                            while (i < src.len) : (i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {
                                if (utility.xmlIsWhitespaceChar(src[i]) or std.mem.startsWith(u8, src[i..], "?>")) {
                                    suspend tt.emitResult(pi_tok_start_index, .pi_tok, .{ .len = i - pi_tok_start_index });
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
                    tt.emitError(0, Error.IncompleteBangToken);
                    break :tokenization;
                }

                switch (src[i]) {
                    '-' => {
                        i += 1;
                        if (i == src.len) {
                            tt.emitError(0, Error.IncompleteBangDashToken);
                            break :tokenization;
                        }

                        switch (src[i]) {
                            '-' => {
                                suspend tt.emitResult(0, .comment_start, {});

                                i += 1;
                                if (i == src.len) break :tokenization;
                                if (std.mem.startsWith(u8, src[i..], "--")) {
                                    if (i + "--".len >= src.len or src[i + "--".len] != '>')
                                        tt.emitError(i - "--".len, Error.InvalidDoubleDashInComment)
                                    else
                                        tt.emitResult("<!--".len, .comment_end, {});
                                    break :tokenization;
                                }

                                seek_double_slash: while (true) {
                                    i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable;

                                    if (i == src.len or std.mem.startsWith(u8, src[i..], "--")) {
                                        suspend tt.emitResult("<!--".len, .comment_text, .{ .len = i - "<!--".len });
                                        if (i == src.len) break :tokenization;
                                        break :seek_double_slash;
                                    }
                                }

                                std.debug.assert(std.mem.startsWith(u8, src[i..], "--"));
                                i += "--".len;

                                if (i == src.len or src[i] != '>') {
                                    tt.emitError(i - "--".len, Error.InvalidDoubleDashInComment);
                                    break :tokenization;
                                }

                                tt.emitResult(i - "--".len, .comment_end, {});
                                break :tokenization;
                            },
                            else => {
                                tt.emitError(i, Error.IncompleteBangDashToken);
                                break :tokenization;
                            },
                        }
                    },
                    '[' => {
                        inline for ("CDATA[") |expected_char| {
                            i += 1;
                            if (i == src.len or src[i] != expected_char) {
                                tt.emitError(i, Error.IncompleteCDataKeyword);
                                break :tokenization;
                            }
                        }
                        suspend tt.emitResult(0, .cdata_start, {});

                        i += 1;
                        if (i == src.len) break :tokenization;
                        if (std.mem.startsWith(u8, src[i..], "]]>")) {
                            tt.emitResult("<![CDATA[".len, .cdata_end, {});
                            break :tokenization;
                        }

                        while (true) {
                            i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
                            if (i == src.len or std.mem.startsWith(u8, src[i..], "]]>")) {
                                suspend tt.emitResult("<![CDATA[".len, .cdata_text, .{ .len = i - "<![CDATA[".len });
                                if (i == src.len) break :tokenization;
                                break;
                            }
                        }

                        std.debug.assert(std.mem.startsWith(u8, src[i..], "]]>"));
                        tt.emitResult(i, .cdata_end, {});
                        break :tokenization;
                    },
                    else => {
                        tt.emitError(i, Error.IncompleteBangToken);
                        break :tokenization;
                    },
                }
            },
            '/' => {
                suspend tt.emitResult(0, .elem_close_start, {});

                tokenize_name: {
                    i += 1;
                    if (i == src.len) break :tokenization;

                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.emitError(i, Error.ExpectedElemCloseNameStartChar);
                        break :tokenization;
                    };
                    i = utility.xmlNextNonNameCharIndexAfter(src, i);

                    suspend tt.emitResult("</".len, .elem_tag_name, .{ .len = i - "</".len });
                    break :tokenize_name;
                }
                i = utility.xmlNextNonWhitespaceCharIndexAfter(src, i);

                if (i == src.len) break :tokenization;
                if (src[i] != '>') {
                    tt.emitError(i, Error.UnexpectedCharacterFollowingElemCloseName);
                    break :tokenization;
                }

                tt.emitResult(i, .elem_tag_end, {});
                break :tokenization;
            },
            else => {
                suspend tt.emitResult(0, .elem_open_start, {});

                tokenize_name: {
                    i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                        tt.emitError(i, Error.ExpectedElemOpenNameStartChar);
                        break :tokenization;
                    };
                    i = utility.xmlNextNonNameCharIndexAfter(src, i);

                    suspend tt.emitResult("<".len, .elem_tag_name, .{ .len = i - "<".len });
                    break :tokenize_name;
                }

                get_attributes: while (true) {
                    i = utility.xmlNextNonWhitespaceCharIndexAfter(src, i);
                    if (i == src.len) break :tokenization;

                    switch (src[i]) {
                        '>' => {
                            tt.emitResult(i, .elem_tag_end, {});
                            break :tokenization;
                        },
                        '/' => {
                            if (i + 1 == src.len) {
                                tt.emitError(i, Error.IncompleteElemCloseInlineTok);
                                break :tokenization;
                            }

                            if (src[i + 1] != '>') {
                                tt.emitError(i, Error.ExpectedRightAngleBracket);
                                break :tokenization;
                            }

                            tt.emitResult(i, .elem_close_inline, {});
                            break :tokenization;
                        },
                        else => {
                            tokenize_attr_name: {
                                const attr_name_start_index = i;
                                i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                                    tt.emitError(i, Error.InvalidCharacter);
                                    break :tokenization;
                                };
                                i = utility.xmlNextNonNameCharIndexAfter(src, i);

                                suspend tt.emitResult(attr_name_start_index, .attr_name, .{ .len = i - attr_name_start_index });
                                break :tokenize_attr_name;
                            }

                            i = utility.xmlNextNonWhitespaceCharIndexAfter(src, i);
                            if (i == src.len) break :tokenization;
                            if (src[i] != '=') {
                                tt.emitError(i, Error.ExpectedAttrEql);
                                break :tokenization;
                            }

                            suspend tt.emitResult(i, .attr_eql, {});

                            i += 1;
                            i = utility.xmlNextNonWhitespaceCharIndexAfter(src, i);
                            if (i == src.len) break :tokenization;

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote: QuoteType = switch (src[i]) {
                                '\"', '\'' => @intToEnum(QuoteType, src[i]),
                                else => {
                                    tt.emitError(i, Error.ExpectedAttrQuote);
                                    break :tokenization;
                                },
                            };

                            suspend switch (quote) {
                                .single => tt.emitResult(i, .attr_quote_single, {}),
                                .double => tt.emitResult(i, .attr_quote_double, {}),
                            };

                            i += 1;

                            get_attr_value: while (true) {
                                if (i == src.len) break :tokenization;
                                if (@enumToInt(quote) == src[i]) break :get_attr_value;

                                if (src[i] == '&') {
                                    suspend tt.emitResult(i, .attr_val_entref_start, {});

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
                                                    tt.emitError(i, Error.InvalidEntrefIdChar);
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

                                            suspend tt.emitResult(entref_id_start_index, .attr_val_entref_id, .{ .len = i - entref_id_start_index });
                                        },
                                        else => {
                                            const entref_id_start_index = i;
                                            i += utility.xmlNameStartCharLengthAt(src, i) orelse {
                                                tt.emitError(i, Error.InvalidEntrefIdNameChar);
                                                break :tokenization;
                                            };
                                            i = utility.xmlNextNonNameCharIndexAfter(src, i);

                                            suspend tt.emitResult(entref_id_start_index, .attr_val_entref_id, .{ .len = i - entref_id_start_index });
                                        },
                                    }

                                    if (i == src.len) break :tokenization;
                                    if (src[i] != ';') {
                                        tt.emitError(i, Error.ExpectedEntrefSemicolon);
                                        break :tokenization;
                                    }

                                    suspend tt.emitResult(i, .attr_val_entref_end, {});

                                    i += 1;
                                    continue :get_attr_value;
                                }

                                const text_value_start_index = i;
                                while (i < src.len and src[i] != @enumToInt(quote) and src[i] != '&') : (i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable) {}

                                suspend tt.emitResult(text_value_start_index, .attr_val_text, .{ .len = i - text_value_start_index });
                            }

                            std.debug.assert(quote == @intToEnum(QuoteType, src[i]));

                            suspend switch (quote) {
                                .single => tt.emitResult(i, .attr_quote_single, {}),
                                .double => tt.emitResult(i, .attr_quote_double, {}),
                            };

                            i += 1;
                            if (i == src.len) break :tokenization;
                            if (src[i] != '>' and
                                src[i] != '/' and
                                !utility.xmlIsWhitespaceChar(src[i]))
                            {
                                tt.emitError(i, Error.InvalidCharacter);
                                break :tokenization;
                            }

                            continue :get_attributes;
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

fn emitResult(tt: *TagTokenizer, index: usize, comptime id: Tok.Id, expr: anytype) void {
    std.debug.assert(tt.tok.* == null);
    tt.tok.* = Tok.init(index, id, expr);
}

fn emitError(tt: *TagTokenizer, index: usize, code: Error) void {
    tt.emitResult(index, .err, .{ .code = code });
}

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

    fn expectPiOpen(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_open, tok.info);
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
    fn expectPiStr(test_tt: *TestTagTokenizer, text: []const u8) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_str, tok.info);

        const actual_slice = tok.slice(test_tt.src);

        try std.testing.expect(switch (actual_slice[0]) {
            '\'', '\"' => true,
            else => false,
        });
        try std.testing.expect(0 != (actual_slice.len - 1));
        try std.testing.expectEqual(actual_slice[0], actual_slice[actual_slice.len - 1]);

        const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ actual_slice[0..1], text, actual_slice[actual_slice.len - 1 ..][0..1] });
        defer std.testing.allocator.free(expected_slice);

        try std.testing.expectEqualStrings(expected_slice, actual_slice);
    }
    fn expectPiClose(test_tt: *TestTagTokenizer) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_close, tok.info);
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
        try std.testing.expect(switch (tok.info) {
            .attr_quote_single,
            .attr_quote_double,
            => true,
            else => false,
        });
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

        pub fn expectPiStart(test_tt: *TestTagTokenizer, target_name: []const u8) !void {
            try test_tt.expectPiOpen();
            try test_tt.expectPiTarget(target_name);
        }

        const PiTokOrStr = union(enum) { tok: []const u8, str: []const u8 };
        pub fn expectPi(test_tt: *TestTagTokenizer, target_name: []const u8, instrs: []const PiTokOrStr) !void {
            try test_tt.expectPiStart(target_name);
            for (instrs) |instr| try switch (instr) {
                .tok => |tok| test_tt.expectPiTok(tok),
                .str => |str| test_tt.expectPiStr(str),
            };
            try test_tt.expectPiClose();
        }

        pub fn expectComment(test_tt: *TestTagTokenizer, text: ?[]const u8) !void {
            try test_tt.expectCommentStart();
            if (text) |txt| try test_tt.expectCommentText(txt);
            try test_tt.expectCommentEnd();
        }

        pub fn expectCData(test_tt: *TestTagTokenizer, text: ?[]const u8) !void {
            try test_tt.expectCDataStart();
            if (text) |txt| try test_tt.expectCDataText(txt);
            try test_tt.expectCDataEnd();
        }
    };
};

test "TagTokenizer Tok Format" {
    var tt = TestTagTokenizer{};

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
    var tt = TestTagTokenizer{};
    tt.reset("").unwrap() catch unreachable;
    try tt.expectNull();
}

test "TagTokenizer Expect Left Angle Bracket" {
    var tt = TestTagTokenizer{};

    tt.reset("a").unwrap() catch unreachable;
    try tt.expectErr(Error.ExpectedLeftAngleBracket);
    try tt.expectNull();
}

test "TagTokenizer Incomplete" {
    var tt = TestTagTokenizer{};

    tt.reset("<!").unwrap() catch unreachable;
    try tt.expectErr(Error.IncompleteBangToken);
    try tt.expectNull();

    tt.reset("<! ").unwrap() catch unreachable;
    try tt.expectErr(Error.IncompleteBangToken);
    try tt.expectNull();

    tt.reset("<!-").unwrap() catch unreachable;
    try tt.expectErr(Error.IncompleteBangDashToken);
    try tt.expectNull();

    tt.reset("<!- ").unwrap() catch unreachable;
    try tt.expectErr(Error.IncompleteBangDashToken);
    try tt.expectNull();

    const @"[CDATA" = "[CDATA";
    inline for ([_]void{undefined} ** @"[CDATA".len) |_, i| {
        tt.reset("<!" ++ (@"[CDATA"[0 .. i + 1])).unwrap() catch unreachable;
        try tt.expectErr(Error.IncompleteCDataKeyword);
        try tt.expectNull();
    }
}

test "TagTokenizer PI" {
    var tt = TestTagTokenizer{};

    tt.reset("<?0").unwrap() catch unreachable;
    try tt.expectPiOpen();
    try tt.expectErr(Error.InvalidPiTargetStartChar);
    try tt.expectNull();

    tt.reset("<?a").unwrap() catch unreachable;
    try tt.expectPiOpen();
    try tt.expectPiTarget("a");
    try tt.expectNull();

    tt.reset("<?a?").unwrap() catch unreachable;
    try tt.expectPiOpen();
    try tt.expectPiTarget("a");
    try tt.expectErr(Error.ExpectedWhitespaceAfterPiTarget);
    try tt.expectNull();

    tt.reset("<?a?>").unwrap() catch unreachable;
    try tt.expectPi("a", &.{});
    try tt.expectNull();

    tt.reset("<?a ?>").unwrap() catch unreachable;
    try tt.expectPi("a", &.{});
    try tt.expectNull();

    tt.reset("<?abc d?>").unwrap() catch unreachable;
    try tt.expectPi("abc", &.{.{ .tok = "d" }});
    try tt.expectNull();

    tt.reset("<?abc def = ''?>").unwrap() catch unreachable;
    try tt.expectPi("abc", &.{
        .{ .tok = "def" },
        .{ .tok = "=" },
        .{ .str = "" },
    });
    try tt.expectNull();

    tt.reset("<?abc def = \"g\"?>").unwrap() catch unreachable;
    try tt.expectPi("abc", &.{
        .{ .tok = "def" },
        .{ .tok = "=" },
        .{ .str = "g" },
    });
    try tt.expectNull();

    tt.reset("<?abc def = 'ghi'\"\" ?>").unwrap() catch unreachable;
    try tt.expectPi("abc", &.{
        .{ .tok = "def" },
        .{ .tok = "=" },
        .{ .str = "ghi" },
        .{ .str = "" },
    });
    try tt.expectNull();
}

test "TagTokenizer Comment" {
    var tt = TestTagTokenizer{};

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
    try tt.expectComment(null);
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
    try tt.expectComment("- ");
    try tt.expectNull();

    tt.reset("<!-- - -->").unwrap() catch unreachable;
    try tt.expectComment(" - ");
    try tt.expectNull();
}

test "TagTokenizer CDATA" {
    var tt = TestTagTokenizer{};

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
    try tt.expectCData(null);
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
        try tt.expectCData(maybe_ambiguous_content);
        try tt.expectNull();
    }

    tt.reset("<![CDATA[a]]>").unwrap() catch unreachable;
    try tt.expectCData("a");
    try tt.expectNull();

    tt.reset("<![CDATA[ foobar ]]>").unwrap() catch unreachable;
    try tt.expectCData(" foobar ");
    try tt.expectNull();
}

test "TagTokenizer Element Close" {
    var tt = TestTagTokenizer{};

    tt.reset("</").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectNull();

    tt.reset("</-").unwrap() catch unreachable;
    try tt.expectElemCloseStart();
    try tt.expectErr(Error.ExpectedElemCloseNameStartChar);
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
    try tt.expectErr(Error.UnexpectedCharacterFollowingElemCloseName);
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
    var tt = TestTagTokenizer{};

    tt.reset("<").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectNull();

    tt.reset("<0").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectErr(Error.ExpectedElemOpenNameStartChar);
    try tt.expectNull();

    tt.reset("<a").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("a");
    try tt.expectNull();

    tt.reset("<foo").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectNull();

    tt.reset("<foo -").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.InvalidCharacter);
    try tt.expectNull();

    tt.reset("<foo/").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.IncompleteElemCloseInlineTok);
    try tt.expectNull();

    tt.reset("<foo /").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.IncompleteElemCloseInlineTok);
    try tt.expectNull();

    tt.reset("<foo /-").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectErr(Error.ExpectedRightAngleBracket);
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
}

test "TagTokenizer Element Open Attributes" {
    var tt = TestTagTokenizer{};

    tt.reset("<foo bar ").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectNull();

    tt.reset("<foo bar />").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectErr(Error.ExpectedAttrEql);
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
    try tt.expectErr(Error.ExpectedAttrQuote);
    try tt.expectNull();

    tt.reset("<foo bar='").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
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

    tt.reset("<foo bar='&").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
    try tt.expectAttrValEntrefStart();
    try tt.expectNull();

    tt.reset("<foo bar='&0").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
    try tt.expectAttrValEntrefStart();
    try tt.expectErr(Error.InvalidEntrefIdNameChar);
    try tt.expectNull();

    tt.reset("<foo bar='&baz").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
    try tt.expectAttrValEntrefStart();
    try tt.expectAttrValEntrefId("baz");
    try tt.expectNull();

    tt.reset("<foo bar='&baz ").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
    try tt.expectAttrValEntrefStart();
    try tt.expectAttrValEntrefId("baz");
    try tt.expectErr(Error.ExpectedEntrefSemicolon);
    try tt.expectNull();

    tt.reset("<foo bar='&baz;'/>").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttr("bar", &.{.{ .entref = "baz" }});
    try tt.expectElemCloseInline();
    try tt.expectNull();

    tt.reset("<foo bar='&#Z").unwrap() catch unreachable;
    try tt.expectElemOpenStart();
    try tt.expectElemTagName("foo");
    try tt.expectAttrName("bar");
    try tt.expectAttrEql();
    try tt.expectAttrQuote();
    try tt.expectAttrValEntrefStart();
    try tt.expectErr(Error.InvalidEntrefIdChar);
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
