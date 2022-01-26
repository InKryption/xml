//! Struct which can be default initialized, but must
//! have its `reset` or `resetUnchecked` function called on it before
//! being able to produce valid results with its `next` function.
//! Purpose is to tokenize XML tags. That is, anything beginning with "<".

const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const debug = std.debug;
const testing = std.testing;
const unicode = std.unicode;

const print = debug.print;
const assert = debug.assert;

const util = @import("util.zig");

const TagTokenizer = @This();
state: @Frame(TagTokenizer.tokenize) = undefined,
tok: *?Tok = undefined,

src: []const u8 = undefined,
i: usize = undefined,
debug_valid_resume: util.DebugType(bool) = util.debugValue(false),

pub const Error = error{
    UnexpectedEof,
    InvalidCharacter,
    InvalidDoubleDashInComment,
};

pub fn next(tt: *TagTokenizer) ?Tok {
    var tok: ?Tok = null;

    tt.tok = &tok;
    defer tt.tok = undefined;

    if (util.debug_mode) assert(!tt.debug_valid_resume);
    resume tt.state;
    if (util.debug_mode) {
        assert(tt.debug_valid_resume);
        tt.debug_valid_resume = false;
    }
    return tok;
}

pub const ResetResult = union(enum) {
    ok,
    err: ErrContext,

    pub fn orElse(result: ResetResult, comptime otherwise: ?fn (ErrContext) void) ResetResult.Error!void {
        switch (result) {
            .ok => {},
            .err => |err| {
                if (otherwise) |func| func(err);
                return err.code;
            },
        }
    }

    pub fn assumeOk(result: ResetResult, comptime otherwise: ?fn (ErrContext) noreturn) void {
        switch (result) {
            .ok => {},
            .err => |err| if (otherwise) |func|
                func(err)
            else
                unreachable,
        }
    }

    pub fn assumeOkPanic(err: ErrContext) noreturn {
        debug.panic("Encountered error '{}' at index '{}'.", .{ err.code, err.index });
    }

    pub const Error = error{
        Utf8ByteSequenceLengthTooLong,

        Utf8InvalidStartByte,
        Utf8ExpectedContinuation,
        Utf8OverlongEncoding,
        Utf8EncodesSurrogateHalf,
        Utf8CodepointTooLarge,
    };

    pub const ErrContext = struct {
        tt: *TagTokenizer,
        code: ResetResult.Error,
        index: usize,
    };
};

pub fn reset(tt: *TagTokenizer, src: []const u8) ResetResult {
    tt.resetUnchecked(src);

    var i: usize = 0;
    while (i < src.len) {
        const cp_len = unicode.utf8ByteSequenceLength(src[i]) catch |err| return @unionInit(ResetResult, "err", ResetResult.ErrContext{
            .tt = tt,
            .code = err,
            .index = i,
        });
        if (i + cp_len > src.len) return @unionInit(ResetResult, "err", ResetResult.ErrContext{
            .tt = tt,
            .code = ResetResult.Error.Utf8ByteSequenceLengthTooLong,
            .index = i,
        });
        if (unicode.utf8Decode(src[i .. i + cp_len])) |_| {} else |err| return @unionInit(ResetResult, "err", ResetResult.ErrContext{
            .tt = tt,
            .code = err,
            .index = i,
        });
        i += cp_len;
    }

    return .ok;
}

pub fn resetUnchecked(self: *TagTokenizer, src: []const u8) void {
    self.* = .{};
    self.state = async self.tokenize(src);
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
    tt.i = 0;
    tt.src = src;

    suspend {}
    tt.assertUnchanged(0, src);

    tokenization: {
        assert(src.len != 0);
        assert(src[0] == '<');

        tt.i += 1;
        if (tt.i == src.len) {
            suspend tt.setResult(Tok.init(0, .elem_open_start, {}));
            tt.assertUnchanged(1, src);

            assert(tt.unexpectedEof());
            break :tokenization;
        }

        switch (src[tt.i]) {
            '?' => {
                suspend tt.setResult(Tok.init(0, .pi_start, {}));
                tt.assertUnchanged(1, src);

                tt.i += 1;
                if (tt.unexpectedEof()) break :tokenization;

                if (tt.invalidNameStartChar()) break :tokenization;
                while (tt.i < src.len and isValidNameChar(tt.currentCodepoint())) : (tt.i += tt.currentCodepointLen()) {}

                set_result: {
                    const i_before_suspend = tt.i;
                    suspend tt.setResult(Tok.init("<?".len, .pi_target, .{ .len = tt.i - "<?".len }));
                    tt.assertUnchanged(i_before_suspend, src);
                    break :set_result;
                }

                if (tt.unexpectedEof()) break :tokenization;
                if (mem.startsWith(u8, src[tt.i..], "?>")) {
                    tt.setResult(Tok.init(tt.i, .pi_end, {}));
                    break :tokenization;
                }
                if (!isWhitespaceChar(src[tt.i])) {
                    tt.setInvalidChararcter();
                    break :tokenization;
                }

                get_tokens: while (true) {
                    tt.i = tt.nextNonWhitespaceCharIndex();
                    if (tt.unexpectedEof()) break :tokenization;

                    if (mem.startsWith(u8, src[tt.i..], "?>")) {
                        tt.setResult(Tok.init(tt.i, .pi_end, {}));
                        break :tokenization;
                    }
                    switch (src[tt.i]) {
                        '\'', '\"' => {
                            const pi_str_start_index = tt.i;
                            tt.i += 1;

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote = @intToEnum(QuoteType, src[pi_str_start_index]);

                            while (tt.i < src.len) : (tt.i += tt.currentCodepointLen()) {
                                if (src[tt.i] == @enumToInt(quote)) {
                                    assert(tt.currentCodepoint() == @enumToInt(quote));
                                    tt.i += 1;

                                    set_result: {
                                        const i_before_suspend = tt.i;
                                        suspend tt.setResult(Tok.init(pi_str_start_index, .pi_str, .{ .len = tt.i - pi_str_start_index }));
                                        tt.assertUnchanged(i_before_suspend, src);
                                        break :set_result;
                                    }
                                    continue :get_tokens;
                                }
                            }
                        },
                        else => {
                            const pi_tok_start_index = tt.i;

                            while (tt.i < src.len) : (tt.i += tt.currentCodepointLen()) {
                                if (isWhitespaceChar(tt.currentCodepoint()) or mem.startsWith(u8, src[tt.i..], "?>")) {
                                    set_result: {
                                        const i_before_suspend = tt.i;
                                        suspend tt.setResult(Tok.init(pi_tok_start_index, .pi_tok, .{ .len = tt.i - pi_tok_start_index }));
                                        tt.assertUnchanged(i_before_suspend, src);
                                        break :set_result;
                                    }
                                    continue :get_tokens;
                                }
                            }
                        },
                    }
                }
            },
            '!' => {
                tt.i += 1;
                if (tt.unexpectedEof()) break :tokenization;

                switch (src[tt.i]) {
                    '-' => {
                        tt.i += 1;
                        if (tt.unexpectedEof()) break :tokenization;

                        switch (src[tt.i]) {
                            '-' => {
                                suspend tt.setResult(Tok.init(0, .comment_start, {}));
                                tt.assertUnchanged(3, src);

                                tt.i += 1;
                                if (tt.unexpectedEof()) break :tokenization;
                                if (mem.startsWith(u8, src[tt.i..], "--")) {
                                    if (tt.i + "--".len >= src.len or src[tt.i + "--".len] != '>') {
                                        tt.setResult(Tok.init(tt.i - "--".len, .err, .{ .code = Error.InvalidDoubleDashInComment }));
                                    } else {
                                        tt.setResult(Tok.init("<!--".len, .comment_end, {}));
                                    }
                                    break :tokenization;
                                }

                                seek_double_slash: while (true) {
                                    _ = tt.currentCodepoint();
                                    tt.i += tt.currentCodepointLen();

                                    if (tt.i == src.len or mem.startsWith(u8, src[tt.i..], "--")) {
                                        set_result: {
                                            const i_before_suspend = tt.i;
                                            suspend tt.setResult(Tok.init("<!--".len, .comment_text, .{ .len = tt.i - "<!--".len }));
                                            tt.assertUnchanged(i_before_suspend, src);
                                            break :set_result;
                                        }

                                        if (tt.unexpectedEof()) break :tokenization;
                                        break :seek_double_slash;
                                    }
                                }

                                assert(mem.startsWith(u8, src[tt.i..], "--"));
                                tt.i += "--".len;

                                if (tt.i == src.len or src[tt.i] != '>') {
                                    tt.setResult(Tok.init(tt.i - "--".len, .err, .{ .code = Error.InvalidDoubleDashInComment }));
                                    break :tokenization;
                                }

                                tt.setResult(Tok.init(tt.i - "--".len, .comment_end, {}));
                                break :tokenization;
                            },
                            else => {
                                tt.setInvalidChararcter();
                                break :tokenization;
                            },
                        }
                    },
                    '[' => {
                        inline for ("CDATA[") |expected_char| {
                            tt.i += 1;
                            if (tt.unexpectedEof()) break :tokenization;

                            if (src[tt.i] != expected_char) {
                                tt.setInvalidChararcter();
                                break :tokenization;
                            }
                            assert(tt.currentCodepoint() == expected_char);
                            assert(tt.currentCodepointLen() == 1);
                        }

                        set_result: {
                            const i_before_suspend = tt.i;
                            suspend tt.setResult(Tok.init(0, .cdata_start, {}));
                            tt.assertUnchanged(i_before_suspend, src);
                            break :set_result;
                        }

                        tt.i += 1;
                        if (tt.unexpectedEof()) break :tokenization;

                        if (mem.startsWith(u8, src[tt.i..], "]]>")) {
                            tt.setResult(Tok.init("<![CDATA[".len, .cdata_end, {}));
                            break :tokenization;
                        }

                        while (true) {
                            _ = tt.currentCodepoint();
                            tt.i += tt.currentCodepointLen();

                            if (tt.i == src.len or mem.startsWith(u8, src[tt.i..], "]]>")) {
                                set_result: {
                                    const i_before_suspend = tt.i;
                                    suspend tt.setResult(Tok.init("<![CDATA[".len, .cdata_text, .{ .len = tt.i - "<![CDATA[".len }));
                                    tt.assertUnchanged(i_before_suspend, src);
                                    break :set_result;
                                }

                                if (tt.unexpectedEof()) break :tokenization;
                                break;
                            }
                        }

                        assert(mem.startsWith(u8, src[tt.i..], "]]>"));
                        tt.setResult(Tok.init(tt.i, .cdata_end, {}));
                        break :tokenization;
                    },
                    else => {
                        tt.setInvalidChararcter();
                        break :tokenization;
                    },
                }
            },
            '/' => {
                suspend tt.setResult(Tok.init(0, .elem_close_start, {}));
                tt.assertUnchanged(1, src);

                tt.i += 1;
                if (tt.unexpectedEof()) break :tokenization;
                if (tt.invalidNameStartChar()) break :tokenization;

                while (tt.i < src.len and isValidNameChar(tt.currentCodepoint())) : (tt.i += tt.currentCodepointLen()) {}
                set_result: {
                    const i_before_suspend = tt.i;
                    suspend tt.setResult(Tok.init("</".len, .elem_tag_name, .{ .len = tt.i - "</".len }));
                    tt.assertUnchanged(i_before_suspend, src);
                    break :set_result;
                }

                tt.i = tt.nextNonWhitespaceCharIndex();

                if (tt.unexpectedEof()) break :tokenization;
                if (src[tt.i] != '>') {
                    tt.setInvalidChararcter();
                    break :tokenization;
                }

                tt.setResult(Tok.init(tt.i, .elem_tag_end, {}));
                break :tokenization;
            },
            else => {
                suspend tt.setResult(Tok.init(0, .elem_open_start, {}));
                tt.assertUnchanged(1, src);

                if (tt.invalidNameStartChar()) break :tokenization;
                tt.i += tt.currentCodepointLen();

                while (tt.i < src.len and isValidNameChar(tt.currentCodepoint())) : (tt.i += tt.currentCodepointLen()) {}
                set_result: {
                    const i_before_suspend = tt.i;
                    suspend tt.setResult(Tok.init("<".len, .elem_tag_name, .{ .len = tt.i - "<".len }));
                    tt.assertUnchanged(i_before_suspend, src);
                    break :set_result;
                }

                get_attributes: while (true) {
                    tt.i = tt.nextNonWhitespaceCharIndex();
                    if (tt.unexpectedEof()) break :tokenization;

                    switch (src[tt.i]) {
                        '>' => {
                            tt.setResult(Tok.init(tt.i, .elem_tag_end, {}));
                            break :tokenization;
                        },
                        '/' => {
                            if (tt.i + 1 == src.len) {
                                tt.setInvalidChararcter();
                                break :tokenization;
                            }

                            if (src[tt.i + 1] != '>') {
                                tt.setInvalidChararcter();
                                break :tokenization;
                            }

                            tt.setResult(Tok.init(tt.i, .elem_close_inline, {}));
                            break :tokenization;
                        },
                        else => {
                            const attr_name_start_index = tt.i;
                            if (tt.invalidNameStartChar()) break :tokenization;
                            tt.i += tt.currentCodepointLen();
                            while (tt.i < src.len and isValidNameChar(tt.currentCodepoint())) : (tt.i += tt.currentCodepointLen()) {}

                            set_result: {
                                const i_before_suspend = tt.i;
                                suspend tt.setResult(Tok.init(attr_name_start_index, .attr_name, .{ .len = tt.i - attr_name_start_index }));
                                tt.assertUnchanged(i_before_suspend, src);
                                break :set_result;
                            }

                            tt.i = tt.nextNonWhitespaceCharIndex();
                            if (tt.unexpectedEof()) break :tokenization;

                            if (src[tt.i] != '=') {
                                tt.setInvalidChararcter();
                                break :tokenization;
                            }

                            set_result: {
                                const i_before_suspend = tt.i;
                                suspend tt.setResult(Tok.init(tt.i, .attr_eql, {}));
                                tt.assertUnchanged(i_before_suspend, src);
                                break :set_result;
                            }

                            tt.i += 1;
                            tt.i = tt.nextNonWhitespaceCharIndex();
                            if (tt.unexpectedEof()) break :tokenization;

                            const QuoteType = enum(u8) { single = '\'', double = '\"' };
                            const quote: QuoteType = switch (src[tt.i]) {
                                '\"', '\'' => @intToEnum(QuoteType, src[tt.i]),
                                else => {
                                    tt.setInvalidChararcter();
                                    break :tokenization;
                                },
                            };
                            set_result: {
                                const i_before_suspend = tt.i;
                                suspend tt.setResult(Tok.init(tt.i, .attr_quote, {}));
                                tt.assertUnchanged(i_before_suspend, src);
                                break :set_result;
                            }

                            tt.i += 1;

                            get_attr_value: while (true) {
                                if (tt.unexpectedEof()) break :tokenization;

                                if (@enumToInt(quote) == src[tt.i]) {
                                    set_result: {
                                        const i_before_suspend = tt.i;
                                        suspend tt.setResult(Tok.init(tt.i, .attr_quote, {}));
                                        tt.assertUnchanged(i_before_suspend, src);
                                        break :set_result;
                                    }

                                    tt.i += 1;
                                    if (tt.unexpectedEof()) break :tokenization;

                                    if (src[tt.i] != '>' and
                                        src[tt.i] != '/' and
                                        !isWhitespaceChar(src[tt.i]))
                                    {
                                        tt.setInvalidChararcter();
                                        break :tokenization;
                                    }

                                    continue :get_attributes;
                                }

                                if (src[tt.i] == '&') {
                                    set_result: {
                                        const i_before_suspend = tt.i;
                                        suspend tt.setResult(Tok.init(tt.i, .attr_val_entref_start, {}));
                                        tt.assertUnchanged(i_before_suspend, src);
                                        break :set_result;
                                    }

                                    const attr_entref_name_start_index = tt.i;
                                    _ = attr_entref_name_start_index;

                                    tt.i += 1;
                                    if (tt.unexpectedEof()) break :tokenization;
                                    switch (src[tt.i]) {
                                        '#' => {
                                            const entref_id_start_index = tt.i;

                                            tt.i += 1;
                                            if (tt.unexpectedEof()) break :tokenization;

                                            if (mem.startsWith(u8, src[tt.i..], "0x")) tt.i += "0x".len;
                                            if (tt.unexpectedEof()) break :tokenization;

                                            switch (src[tt.i]) {
                                                '0'...'9',
                                                'a'...'f',
                                                'A'...'F',
                                                => {},
                                                else => {
                                                    tt.setInvalidChararcter();
                                                    break :tokenization;
                                                },
                                            }

                                            while (tt.i < src.len) switch (src[tt.i]) {
                                                '0'...'9',
                                                'a'...'f',
                                                'A'...'F',
                                                => tt.i += 1,
                                                else => break,
                                            };

                                            set_result: {
                                                const i_before_suspend = tt.i;
                                                suspend tt.setResult(Tok.init(entref_id_start_index, .attr_val_entref_id, .{ .len = tt.i - entref_id_start_index }));
                                                tt.assertUnchanged(i_before_suspend, src);
                                                break :set_result;
                                            }
                                        },
                                        else => {
                                            const entref_id_start_index = tt.i;
                                            if (tt.invalidNameStartChar()) break :tokenization;
                                            tt.i += tt.currentCodepointLen();

                                            while (tt.i < src.len and isValidNameChar(tt.currentCodepoint())) : (tt.i += tt.currentCodepointLen()) {}
                                            set_result: {
                                                const i_before_suspend = tt.i;
                                                suspend tt.setResult(Tok.init(entref_id_start_index, .attr_val_entref_id, .{ .len = tt.i - entref_id_start_index }));
                                                tt.assertUnchanged(i_before_suspend, src);
                                                break :set_result;
                                            }
                                        },
                                    }

                                    if (tt.unexpectedEof()) break :tokenization;
                                    if (src[tt.i] != ';') {
                                        tt.setInvalidChararcter();
                                        break :tokenization;
                                    }

                                    set_result: {
                                        const i_before_suspend = tt.i;
                                        suspend tt.setResult(Tok.init(tt.i, .attr_val_entref_end, {}));
                                        tt.assertUnchanged(i_before_suspend, src);
                                        break :set_result;
                                    }

                                    tt.i += 1;
                                    continue :get_attr_value;
                                }

                                const text_value_start_index = tt.i;
                                while (tt.i < src.len and src[tt.i] != @enumToInt(quote) and src[tt.i] != '&') : (tt.i += tt.currentCodepointLen()) {}

                                set_result: {
                                    const i_before_suspend = tt.i;
                                    suspend tt.setResult(Tok.init(text_value_start_index, .attr_val_text, .{ .len = tt.i - text_value_start_index }));
                                    tt.assertUnchanged(i_before_suspend, src);
                                    break :set_result;
                                }

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

    while (true) {
        suspend {}
        const i_before_suspend = tt.i;
        tt.assertUnchanged(i_before_suspend, src);
    }
}

fn setResult(tt: *TagTokenizer, tok: Tok) void {
    assert(tt.tok.* == null);
    assert(tt.debug_valid_resume);
    tt.tok.* = tok;
}

fn assertUnchanged(tt: *TagTokenizer, i: usize, src: []const u8) void {
    assert(tt.i == i);
    assert(tt.src.ptr == src.ptr);
    assert(tt.src.len == src.len);
    assert(mem.eql(u8, src, tt.src));
    if (util.debug_mode) {
        assert(!tt.debug_valid_resume);
        tt.debug_valid_resume = true;
    }
}

usingnamespace utility;
const utility = struct {
    /// NOTE: For some reason zig compiler detects a dependency loop if this self parameter is passed by value or by const pointer, so unfortunately have to do this;
    /// the function does not modify anything
    pub fn currentCodepointLen(tt: *TagTokenizer) u3 {
        return unicode.utf8ByteSequenceLength(tt.src[tt.i]) catch unreachable;
    }

    /// NOTE: For some reason zig compiler detects a dependency loop if this self parameter is passed by value or by const pointer, so unfortunately have to do this;
    /// the function does not modify anything
    pub fn currentCodepoint(tt: *TagTokenizer) u21 {
        const cp_len = tt.currentCodepointLen();
        assert(tt.i + cp_len <= tt.src.len);
        return unicode.utf8Decode(tt.src[tt.i .. tt.i + cp_len]) catch unreachable;
    }

    pub fn nextNonWhitespaceCharIndex(tt: *TagTokenizer) usize {
        var new_index: usize = tt.i;
        while (new_index < tt.src.len) {
            const cp_len = unicode.utf8ByteSequenceLength(tt.src[new_index]) catch unreachable;
            assert(new_index + cp_len <= tt.src.len);
            const cp = unicode.utf8Decode(tt.src[new_index .. new_index + cp_len]) catch unreachable;
            if (isWhitespaceChar(cp)) {
                new_index += cp_len;
            } else break;
        }
        return new_index;
    }
};

usingnamespace error_handling;
const error_handling = struct {
    pub fn unexpectedEof(tt: *TagTokenizer) bool {
        assert(tt.src.len != 0);
        assert(tt.i != 0);

        assert(tt.tok.* == null);
        if (tt.i == tt.src.len) {
            tt.setResult(Tok.init(tt.i, .err, .{ .code = Error.UnexpectedEof }));
            return true;
        }
        return false;
    }
    pub fn setInvalidChararcter(tt: *TagTokenizer) void {
        assert(tt.src.len != 0);
        assert(tt.i != 0);

        assert(tt.tok.* == null);
        tt.setResult(Tok.init(tt.i, .err, .{ .code = Error.InvalidCharacter }));
    }
    pub fn invalidNameStartChar(tt: *TagTokenizer) bool {
        assert(tt.src.len != 0);
        assert(tt.i != 0);

        assert(tt.tok.* == null);
        if (!isValidNameStartChar(tt.currentCodepoint())) {
            tt.setResult(Tok.init(tt.i, .err, .{ .code = Error.InvalidCharacter }));
            return true;
        }
        return false;
    }
};

inline fn isWhitespaceChar(cp: u21) bool {
    return switch (cp) {
        ' ', '\t', '\n', '\r' => true,
        else => false,
    };
}

inline fn isValidNameStartChar(cp: u21) bool {
    return switch (cp) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        '\u{c0}'...'\u{d6}',
        '\u{d8}'...'\u{f6}',
        '\u{f8}'...'\u{2ff}',
        '\u{370}'...'\u{37d}',
        '\u{37f}'...'\u{1fff}',
        '\u{200c}'...'\u{200d}',
        '\u{2070}'...'\u{218f}',
        '\u{2c00}'...'\u{2fef}',
        '\u{3001}'...'\u{d7ff}',
        '\u{f900}'...'\u{fdcf}',
        '\u{fdf0}'...'\u{fffd}',
        '\u{10000}'...'\u{effff}',
        => true,
        else => false,
    };
}

inline fn isValidNameChar(cp: u21) bool {
    return isValidNameStartChar(cp) or switch (cp) {
        '-',
        '.',
        '0'...'9',
        '\u{b7}',
        '\u{0300}'...'\u{036f}',
        '\u{203f}'...'\u{2040}',
        => true,
        else => false,
    };
}

const tests = struct {
    fn expectPiStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectPiTarget(tt: *TagTokenizer, name: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_target, tok.info);
        try testing.expectEqualStrings(name, tok.slice(tt.src));
    }
    fn expectPiTok(tt: *TagTokenizer, token: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_tok, tok.info);
        try testing.expectEqualStrings(token, tok.slice(tt.src));
    }
    fn expectPiStr(tt: *TagTokenizer, str: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_str, tok.info);
        try testing.expectEqualStrings(str, tok.slice(tt.src));
    }
    fn expectPiEnd(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.pi_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }

    fn expectCommentStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectCommentText(tt: *TagTokenizer, text: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(tt.src));
    }
    fn expectCommentEnd(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.comment_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }

    fn expectCDataStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectCDataText(tt: *TagTokenizer, text: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(tt.src));
    }
    fn expectCDataEnd(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.cdata_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }

    fn expectElemOpenStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_open_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectElemCloseStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_close_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectElemCloseInline(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_close_inline, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectElemTagEnd(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_tag_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectElemTagName(tt: *TagTokenizer, name: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.elem_tag_name, tok.info);
        try testing.expectEqualStrings(name, tok.slice(tt.src));
    }

    fn expectAttrName(tt: *TagTokenizer, name: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_name, tok.info);
        try testing.expectEqualStrings(name, tok.slice(tt.src));
    }
    fn expectAttrEql(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_eql, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectAttrQuote(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_quote, tok.info);
        try testing.expectEqual(@as(usize, 1), tok.slice(tt.src).len);
        const actual = tok.slice(tt.src)[0];
        try testing.expect(actual == '\"' or actual == '\'');
    }
    fn expectAttrValText(tt: *TagTokenizer, text: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_text, tok.info);
        try testing.expectEqualStrings(text, tok.slice(tt.src));
    }
    fn expectAttrValEntrefStart(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_start, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }
    fn expectAttrValEntrefId(tt: *TagTokenizer, id: []const u8) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_id, tok.info);
        try testing.expectEqualStrings(id, tok.slice(tt.src));
    }
    fn expectAttrValEntrefEnd(tt: *TagTokenizer) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(Tok.Id.attr_val_entref_end, tok.info);
        try testing.expectEqualStrings(tok.expectedSlice().?, tok.slice(tt.src));
    }

    fn expectErr(tt: *TagTokenizer, err: TagTokenizer.Error) !void {
        const tok = tt.next() orelse return error.TestExpectedEqual;
        try testing.expectEqual(TagTokenizer.Tok.Id.err, tok.info);
        try testing.expectEqual(err, tok.info.err.code);
        switch (err) {
            Error.InvalidDoubleDashInComment => try testing.expectEqualStrings("--", tt.src[tok.index .. tok.index + "--".len]),
            else => {},
        }
    }
    fn expectNull(tt: *TagTokenizer) !void {
        try testing.expectEqual(@as(?TagTokenizer.Tok, null), tt.next());
    }

    pub usingnamespace shorthands;
    const shorthands = struct {
        pub fn expectAttrValEntref(tt: *TagTokenizer, id: []const u8) !void {
            try tests.expectAttrValEntrefStart(tt);
            try tests.expectAttrValEntrefId(tt, id);
            try tests.expectAttrValEntrefEnd(tt);
        }

        const AttrVal = union(enum) { text: []const u8, entref: []const u8 };
        pub fn expectAttrVal(tt: *TagTokenizer, segments: []const AttrVal) !void {
            try tests.expectAttrQuote(tt);
            for (segments) |seg| switch (seg) {
                .text => |text| try tests.expectAttrValText(tt, text),
                .entref => |id| try tests.expectAttrValEntref(tt, id),
            };
            try tests.expectAttrQuote(tt);
        }

        pub fn expectAttr(tt: *TagTokenizer, name: []const u8, val: []const AttrVal) !void {
            try tests.expectAttrName(tt, name);
            try tests.expectAttrEql(tt);
            try tests.expectAttrVal(tt, val);
        }
    };
};

test "TagTokenizer Unexpected Eof" {
    var tt = TagTokenizer{};

    tt.reset("<?").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!-").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    inline for ([_]void{undefined} ** "[CDATA".len) |_, i| {
        tt.reset("<!" ++ ("[CDATA"[0 .. i + 1])).assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
        try tests.expectErr(&tt, Error.UnexpectedEof);
        try tests.expectNull(&tt);
    }
}

test "TagTokenizer PI" {
    var tt = TagTokenizer{};

    tt.reset("<?a").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<?a?").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<?a?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?a ?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "a");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc d?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "d");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = ''?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "def");
    try tests.expectPiTok(&tt, "=");
    try tests.expectPiStr(&tt, "''");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = \"g\"?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectPiStart(&tt);
    try tests.expectPiTarget(&tt, "abc");
    try tests.expectPiTok(&tt, "def");
    try tests.expectPiTok(&tt, "=");
    try tests.expectPiStr(&tt, "\"g\"");
    try tests.expectPiEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<?abc def = 'ghi'\"\" ?>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
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
    var tt = TagTokenizer{};

    tt.reset("<!--").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!---").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, "-");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<!----").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!---->").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<!----->").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!-- --->").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, " ");
    try tests.expectErr(&tt, Error.InvalidDoubleDashInComment);
    try tests.expectNull(&tt);

    tt.reset("<!--- -->").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, "- ");
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<!-- - -->").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCommentStart(&tt);
    try tests.expectCommentText(&tt, " - ");
    try tests.expectCommentEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer CDATA" {
    var tt = TagTokenizer{};

    tt.reset("<![CDATA[").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCDataStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "]");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]]").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "]]");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[]]>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
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
        tt.reset("<![CDATA[" ++ maybe_ambiguous_content ++ "]]>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
        try tests.expectCDataStart(&tt);
        try tests.expectCDataText(&tt, maybe_ambiguous_content);
        try tests.expectCDataEnd(&tt);
        try tests.expectNull(&tt);
    }

    tt.reset("<![CDATA[a]]>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, "a");
    try tests.expectCDataEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<![CDATA[ foobar ]]>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectCDataStart(&tt);
    try tests.expectCDataText(&tt, " foobar ");
    try tests.expectCDataEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer Element Close" {
    var tt = TagTokenizer{};

    tt.reset("</").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</a").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</foo").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("</foo ñ").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("</foo>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("</foo >").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("</foo\t >").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemCloseStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);
}

test "TagTokenizer Element Open" {
    var tt = TagTokenizer{};

    tt.reset("<").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<a").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "a");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo/").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo /").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo >").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo />").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar ").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo bar />").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo bar = ").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectAttrEql(&tt);
    try tests.expectErr(&tt, Error.UnexpectedEof);
    try tests.expectNull(&tt);

    tt.reset("<foo bar = />").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttrName(&tt, "bar");
    try tests.expectAttrEql(&tt);
    try tests.expectErr(&tt, Error.InvalidCharacter);
    try tests.expectNull(&tt);

    tt.reset("<foo bar=''/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar=\"baz\"/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .text = "baz" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&baz;'/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "baz" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&#0123456789abcdef;'/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "#0123456789abcdef" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<foo bar='&#0x0123456789abcdef;'/>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "foo");
    try tests.expectAttr(&tt, "bar", &.{.{ .entref = "#0x0123456789abcdef" }});
    try tests.expectElemCloseInline(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='foo&bar;baz'>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");

    try tests.expectAttr(&tt, "B", &.{ .{ .text = "foo" }, .{ .entref = "bar" }, .{ .text = "baz" } });

    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='&foo;bar&baz;'>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");
    try tests.expectAttrName(&tt, "B");
    try tests.expectAttrEql(&tt);
    try tests.expectAttrVal(&tt, &.{ .{ .entref = "foo" }, .{ .text = "bar" }, .{ .entref = "baz" } });
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);

    tt.reset("<A B='&foo;&bar;&baz;'>").assumeOk(TagTokenizer.ResetResult.assumeOkPanic);
    try tests.expectElemOpenStart(&tt);
    try tests.expectElemTagName(&tt, "A");
    try tests.expectAttrName(&tt, "B");
    try tests.expectAttrEql(&tt);
    try tests.expectAttrVal(&tt, &.{ .{ .entref = "foo" }, .{ .entref = "bar" }, .{ .entref = "baz" } });
    try tests.expectElemTagEnd(&tt);
    try tests.expectNull(&tt);
}
