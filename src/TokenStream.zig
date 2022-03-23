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
        pi_start: Len,
        /// indicates a non-string token following 'pi_target', 'pi_str', or 'pi_tok'
        pi_tok: Len,
        /// indicates a string following 'pi_target', 'pi_tok', or 'pi_str'
        pi_str: Len,

        /// indicates '<!--{text}-->'
        comment: Len,

        /// indicates '<![CDATA[{text}]]>'
        cdata: Len,
        /// indicates '{.}+' in between 'elem_open' and 'elem_close' (where the latter is not an inline close).
        text: Len,

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
            name: Name,

            pub const Name = struct { index: usize, len: usize };
        };

        pub fn isAttrVal(tok: Tok.Id) bool {
            return switch (tok) {
                .attr_val_text,
                .attr_val_entref,
                => true,
                else => false,
            };
        }

        /// Returns true for `pi_start`, `pi_tok`, and `pi_str`
        pub fn isPi(self: Tok.Id) bool {
            return switch (self) {
                .pi_start,
                .pi_tok,
                .pi_str,
                => true,
                else => false,
            };
        }
    };

    pub fn slice(tok: Tok, src: []const u8) []const u8 {
        return src[tok.index..][0..tok.len()];
    }

    pub fn piTarget(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .pi_start) return null;
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
        const start = tok.info.elem_close.name.index;
        const end = start + tok.info.elem_close.name.len;
        return src[start..end];
    }

    pub fn elemEntrefId(tok: Tok, src: []const u8) ?[]const u8 {
        if (tok.info != .attr_val_entref) return null;
        const whole = tok.slice(src);
        return whole["&".len .. whole.len - ";".len];
    }

    pub fn len(tok: Tok) usize {
        return switch (tok.info) {
            // zig fmt: off
            .err             => unreachable,
            .whitespace      => |whitespace|      whitespace.len,
            .pi_start        => |pi_start|        pi_start.len,
            .pi_tok          => |pi_tok|          pi_tok.len,
            .pi_str          => |pi_str|          pi_str.len,
            .comment         => |comment|         comment.len,
            .cdata           => |cdata|           cdata.len,
            .text            => |text|            text.len,
            .elem_open       => |elem_open|       elem_open.len,
            .elem_close      => |elem_close|      elem_close.len,
            .attr_name       => |attr_name|       attr_name.len,
            .attr_val_text   => |attr_val_text|   attr_val_text.len,
            .attr_val_entref => |attr_val_entref| attr_val_entref.len,
            // zig fmt: on
        };
    }
};

fn tokenize(ts: *TokenStream, src: []const u8) void {
    const static = struct {
        inline fn processPiToks(tag_tokenizer: *TagTokenizer, ptr_i: *usize) ?Tok {
            return if (tag_tokenizer.next()) |pi_instr| blk: {
                const start = ptr_i.* + pi_instr.index;
                break :blk switch (pi_instr.info) {
                    .pi_tok => Tok.init(start, .pi_tok, .{ .len = pi_instr.info.pi_tok.len }),
                    .pi_str => Tok.init(start, .pi_str, .{ .len = pi_instr.info.pi_str.len }),
                    .pi_close => ret_null: {
                        ptr_i.* += pi_instr.index + pi_instr.info.cannonicalSlice().?.len;
                        std.debug.assert(tag_tokenizer.next() == null);
                        break :ret_null null;
                    },
                    else => unreachable,
                };
            } else std.debug.todo("Emit error.");
        }

        inline fn processCommentTok(tag_tokenizer: *TagTokenizer, ptr_i: *usize) Tok {
            const comment_text = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
            if (comment_text.info != .comment_text) std.debug.todo("Emit error.");

            const comment_end = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
            if (comment_end.info != .comment_end) std.debug.todo("Emit error.");

            const start = ptr_i.*;
            const len: usize = comment_end.index + comment_end.info.cannonicalSlice().?.len;
            ptr_i.* += len;

            std.debug.assert(tag_tokenizer.next() == null);
            return Tok.init(start, .comment, .{ .len = len });
        }

        inline fn processAttrValToks(tag_tokenizer: *TagTokenizer, i: usize) ?Tok {
            return if (tag_tokenizer.next()) |attr_val_or_quote| switch (attr_val_or_quote.info) {
                .attr_val_text => Tok.init(i + attr_val_or_quote.index, .attr_val_text, .{
                    .len = attr_val_or_quote.info.attr_val_text.len,
                }),

                .attr_val_entref_start => blk: {
                    const attr_val_entref_id = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (attr_val_entref_id.info != .attr_val_entref_id) std.debug.todo("Emit error.");

                    const attr_val_entref_end = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (attr_val_entref_end.info != .attr_val_entref_end) std.debug.todo("Emit error.");

                    const start = i + attr_val_or_quote.index;
                    const end = i + attr_val_entref_end.index + attr_val_entref_end.info.cannonicalSlice().?.len;
                    break :blk Tok.init(start, .attr_val_entref, .{ .len = end - start });
                },

                .attr_quote_double,
                .attr_quote_single,
                => null,

                else => unreachable,
            } else std.debug.todo("Emit error.");
        }

        inline fn calculateElemCloseInlineTok(
            tag_tokenizer: *TagTokenizer,
            elem_open: Tok,
            elem_close_inline: TagTokenizer.Tok,
            ptr_i: *usize,
        ) Tok {
            const len: usize = elem_close_inline.index + elem_close_inline.info.cannonicalSlice().?.len;
            const result = Tok.init(ptr_i.*, .elem_close, Tok.Info.ElementClose{
                .len = len,
                .name = Tok.Info.ElementClose.Name{
                    .index = elem_open.index + "<".len,
                    .len = elem_open.info.elem_open.len - 1,
                },
            });
            ptr_i.* += len;

            std.debug.assert(tag_tokenizer.next() == null);
            return result;
        }
    };

    var i: usize = 0;
    var tag_tokenizer = TagTokenizer{};
    suspend {}

    tokenization: {
        prolog: while (true) {
            i = utility.xml.nextNonWhitespaceCharIndexAfter(src, i);
            if (i == src.len) break :tokenization;
            if (src[i] != '<') std.debug.todo("Emit error.");

            tag_tokenizer.resetUnchecked(src[i..]);
            if (tag_tokenizer.next()) |first_tag| switch (first_tag.info) {
                .pi_open => {
                    const pi_target = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (pi_target.info != .pi_target) std.debug.todo("Emit error.");

                    suspend ts.emitResult(i, .pi_start, .{ .len = pi_target.index + pi_target.info.pi_target.len });
                    while (static.processPiToks(&tag_tokenizer, &i)) |tok| {
                        suspend ts.emitResultValue(tok);
                    }
                },
                .comment_start => {
                    suspend ts.emitResultValue(static.processCommentTok(&tag_tokenizer, &i));
                },
                .cdata_start => std.debug.todo("Emit error."),
                .elem_open_start => tokenize_elem_open: {
                    const elem_tag_name = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (elem_tag_name.info != .elem_tag_name) std.debug.todo("Emit error.");

                    const elem_open = Tok.init(i, .elem_open, .{ .len = elem_tag_name.index + elem_tag_name.info.elem_tag_name.len });
                    suspend ts.emitResult(i, .elem_open, .{ .len = elem_open.info.elem_open.len });

                    while (true) {
                        const next_tag = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                        switch (next_tag.info) {
                            .attr_name => |attr_name| {
                                const attr_eql = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                                if (attr_eql.info != .attr_eql) std.debug.todo("Emit error.");

                                suspend ts.emitResult(i + next_tag.index, .attr_name, .{ .len = attr_name.len });

                                const attr_quote = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                                if (!attr_quote.info.isAttrQuote()) std.debug.todo("Emit error.");

                                while (static.processAttrValToks(&tag_tokenizer, i)) |tok| {
                                    suspend ts.emitResultValue(tok);
                                }
                            },
                            .elem_close_inline => {
                                suspend ts.emitResultValue(static.calculateElemCloseInlineTok(&tag_tokenizer, elem_open, next_tag, &i));
                                break :tokenize_elem_open;
                            },
                            .elem_tag_end => {
                                i += next_tag.index + next_tag.info.cannonicalSlice().?.len;
                                std.debug.assert(tag_tokenizer.next() == null);
                                break :prolog;
                            },
                            else => unreachable,
                        }
                    }
                },
                .elem_close_start => std.debug.todo("Emit error."),
                .err => |err| switch (err.code) {
                    error.ExpectedLeftAngleBracket => std.debug.todo("Emit error."),
                    else => unreachable,
                },
                else => unreachable,
            } else break :tokenization;
        }

        // incremented for each elem_open, and decremented for each elem_close.
        // starts at 1 since reaching the control flow reaching here means that the prolog block
        // ended with an elem_open.
        var depth: usize = 1;
        document: while (true) {
            if (depth == 0) {
                break :document;
            }
            if (i == src.len) {
                std.debug.todo("Emit error.");
                break :document;
            }
            if (src[i] != '<') {
                const start = i;
                i = utility.xml.nextNonWhitespaceCharIndexAfter(src, i);

                if (i < src.len and src[i] == '<') {
                    suspend ts.emitResult(start, .whitespace, .{ .len = i - start });
                } else {
                    while (i < src.len) {
                        if (src[i] == '<') break;
                        i += std.unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
                    } else std.debug.todo("Emit error.");

                    suspend ts.emitResult(start, .text, .{ .len = i - start });
                }
            }

            tag_tokenizer.resetUnchecked(src[i..]);
            const first_tag = tag_tokenizer.next().?;
            switch (first_tag.info) {
                .pi_open => {
                    const pi_target = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (pi_target.info != .pi_target) std.debug.todo("Emit error.");

                    suspend ts.emitResult(i, .pi_start, .{ .len = pi_target.index + pi_target.info.pi_target.len });
                    while (static.processPiToks(&tag_tokenizer, &i)) |tok| {
                        suspend ts.emitResultValue(tok);
                    }
                },
                .comment_start => {
                    suspend ts.emitResultValue(static.processCommentTok(&tag_tokenizer, &i));
                },
                .cdata_start => {
                    const cdata_text = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (cdata_text.info != .cdata_text) std.debug.todo("Emit error.");

                    const cdata_end = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (cdata_end.info != .cdata_end) std.debug.todo("Emit error.");

                    const len: usize = cdata_end.index + cdata_end.info.cannonicalSlice().?.len;
                    suspend ts.emitResult(i, .cdata, .{ .len = len });
                    i += len;
                },
                .elem_open_start => tokenize_elem_open: {
                    depth += 1;

                    const elem_tag_name = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (elem_tag_name.info != .elem_tag_name) std.debug.todo("Emit error.");

                    const elem_open = Tok.init(i, .elem_open, .{ .len = elem_tag_name.index + elem_tag_name.info.elem_tag_name.len });
                    suspend ts.emitResult(i, .elem_open, .{ .len = elem_open.info.elem_open.len });

                    while (true) {
                        const next_tag = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                        switch (next_tag.info) {
                            .attr_name => |attr_name| {
                                const attr_eql = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                                if (attr_eql.info != .attr_eql) std.debug.todo("Emit error.");

                                suspend ts.emitResult(i + next_tag.index, .attr_name, .{ .len = attr_name.len });

                                const attr_quote = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                                if (!attr_quote.info.isAttrQuote()) std.debug.todo("Emit error.");

                                while (static.processAttrValToks(&tag_tokenizer, i)) |tok| {
                                    suspend ts.emitResultValue(tok);
                                }
                            },
                            .elem_close_inline => {
                                depth -= 1;
                                suspend ts.emitResultValue(static.calculateElemCloseInlineTok(&tag_tokenizer, elem_open, next_tag, &i));
                                break :tokenize_elem_open;
                            },
                            .elem_tag_end => {
                                i += next_tag.index + next_tag.info.cannonicalSlice().?.len;
                                std.debug.assert(tag_tokenizer.next() == null);
                                break :tokenize_elem_open;
                            },
                            else => unreachable,
                        }
                    }
                },
                .elem_close_start => {
                    depth -= 1;

                    const elem_tag_name = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (elem_tag_name.info != .elem_tag_name) std.debug.todo("Emit error.");

                    const elem_tag_end = tag_tokenizer.next() orelse std.debug.todo("Emit error.");
                    if (elem_tag_end.info != .elem_tag_end) std.debug.todo("Emit error.");

                    const len = elem_tag_end.index + elem_tag_end.info.cannonicalSlice().?.len;
                    suspend ts.emitResult(i, .elem_close, Tok.Info.ElementClose{ .len = len, .name = .{
                        .index = i + elem_tag_name.index,
                        .len = elem_tag_name.info.elem_tag_name.len,
                    } });

                    i += len;
                    std.debug.assert(tag_tokenizer.next() == null);
                },
                .err => |err| switch (err.code) {
                    error.ExpectedLeftAngleBracket => std.debug.todo("Emit error."),
                    else => unreachable,
                },
                else => unreachable,
            }
        }
    }

    while (true) {
        suspend {}
    }
}

fn emitResultValue(ts: *TokenStream, tok: Tok) void {
    std.debug.assert(ts.tok.* == null);
    ts.tok.* = tok;
}

fn emitResult(ts: *TokenStream, index: usize, comptime id: Tok.Id, expr: anytype) void {
    ts.emitResultValue(Tok.init(index, id, expr));
}

fn emitError(ts: *TokenStream, index: usize, code: Error) void {
    ts.emitResult(index, .err, .{ .code = code });
}

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

    fn expectCdata(test_ts: *TestTokenStream, text: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.cdata, tok.info);
        try std.testing.expectEqualStrings(text, tok.cdataText(test_ts.src).?);

        const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ "<![CDATA[", text, "]]>" });
        defer std.testing.allocator.free(expected_slice);

        try std.testing.expectEqualStrings(expected_slice, tok.slice(test_ts.src));
    }

    fn expectElemOpen(test_ts: *TestTokenStream, name: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.elem_open, tok.info);
        try std.testing.expectEqualStrings(name, tok.elemOpenName(test_ts.src).?);

        const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ "<", name });
        defer std.testing.allocator.free(expected_slice);

        try std.testing.expectEqualStrings(expected_slice, tok.slice(test_ts.src));
    }

    fn expectElemClose(test_ts: *TestTokenStream, name: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.elem_close, tok.info);
        try std.testing.expectEqualStrings(name, tok.elemCloseName(test_ts.src).?);
    }

    fn expectAttrName(test_ts: *TestTokenStream, name: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.attr_name, tok.info);
        try std.testing.expectEqualStrings(name, tok.slice(test_ts.src));
    }
    fn expectAttrValText(test_ts: *TestTokenStream, text: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.attr_val_text, tok.info);
        try std.testing.expectEqualStrings(text, tok.slice(test_ts.src));
    }
    fn expectAttrValEntref(test_ts: *TestTokenStream, entref_id: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.attr_val_entref, tok.info);
        try std.testing.expectEqualStrings(entref_id, tok.elemEntrefId(test_ts.src).?);

        const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ "&", entref_id, ";" });
        defer std.testing.allocator.free(expected_slice);

        try std.testing.expectEqualStrings(expected_slice, tok.slice(test_ts.src));
    }

    fn expectWhitespace(test_ts: *TestTokenStream, whitespace: []const u8) !void {
        validate_whitespace: {
            for (whitespace) |char| switch (char) {
                ' ', '\t', '\n', '\r' => continue,
                else => @panic("Non-space character found in 'whitespace' argument to expectWhitespace.\n"),
            };
            break :validate_whitespace;
        }
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.whitespace, tok.info);
        try std.testing.expectEqualStrings(whitespace, tok.slice(test_ts.src));
    }

    fn expectText(test_ts: *TestTokenStream, text: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.text, tok.info);
        try std.testing.expectEqualStrings(text, tok.slice(test_ts.src));
    }

    fn expectPiStart(test_ts: *TestTokenStream, name: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_start, tok.info);
        try std.testing.expectEqualStrings(name, tok.piTarget(test_ts.src).?);

        const expected_slice = try std.mem.concat(std.testing.allocator, u8, &.{ "<?", name });
        defer std.testing.allocator.free(expected_slice);

        try std.testing.expectEqualStrings(expected_slice, tok.slice(test_ts.src));
    }
    fn expectPiTok(test_ts: *TestTokenStream, slice: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_tok, tok.info);
        try std.testing.expectEqualStrings(slice, tok.slice(test_ts.src));
    }
    fn expectPiStr(test_ts: *TestTokenStream, text: []const u8) !void {
        const tok = test_ts.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.pi_str, tok.info);
        try std.testing.expectEqualStrings(text, tok.piStrText(test_ts.src).?);

        const actual_slice = tok.slice(test_ts.src);

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

    fn expectErr(test_tt: *TestTokenStream, err: TokenStream.Error) !void {
        const tok = test_tt.next() orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(Tok.Id.err, tok.info);
        try std.testing.expectEqual(err, tok.info.err.code);
    }
    fn expectNull(test_tt: *TestTokenStream) !void {
        try std.testing.expectEqual(@as(?Tok, null), test_tt.next());
    }
};

test "TokenStream Emptiness" {
    var ts = TestTokenStream{};

    ts.reset("").unwrap() catch unreachable;
    try ts.expectNull();

    ts.reset("<empty/>").unwrap() catch unreachable;
    try ts.expectElemOpen("empty");
    try ts.expectElemClose("empty");
    try ts.expectNull();

    ts.reset("<empty></empty>").unwrap() catch unreachable;
    try ts.expectElemOpen("empty");
    try ts.expectElemClose("empty");
    try ts.expectNull();
}

test "TokenStream Comment & PI" {
    var ts = TestTokenStream{};

    ts.reset("<!-- foo -->").unwrap() catch unreachable;
    try ts.expectComment(" foo ");
    try ts.expectNull();

    ts.reset("<!-- foo --><!--bar-->").unwrap() catch unreachable;
    try ts.expectComment(" foo ");
    try ts.expectComment("bar");
    try ts.expectNull();

    ts.reset("<?foo?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectNull();

    ts.reset("<?foo?><?bar ?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectPiStart("bar");
    try ts.expectNull();

    ts.reset("<?foo bar?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectPiTok("bar");
    try ts.expectNull();

    ts.reset("<?foo bar 'baz'?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectPiTok("bar");
    try ts.expectPiStr("baz");
    try ts.expectNull();

    ts.reset("<!--foo--><?bar?><!--baz-->").unwrap() catch unreachable;
    try ts.expectComment("foo");
    try ts.expectPiStart("bar");
    try ts.expectComment("baz");
    try ts.expectNull();

    ts.reset("<?foo?><!--bar--><?baz?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectComment("bar");
    try ts.expectPiStart("baz");
    try ts.expectNull();

    // NOTE: whitespace in prolog is ignored.
    // whitespace tokens are emitted if within an element (depth > 0)
    ts.reset("<!--foo-->\n<?bar ?>\n<!--baz-->").unwrap() catch unreachable;
    try ts.expectComment("foo");
    try ts.expectPiStart("bar");
    try ts.expectComment("baz");
    try ts.expectNull();

    ts.reset("<?foo?>\n<!--bar-->\n<?baz?>").unwrap() catch unreachable;
    try ts.expectPiStart("foo");
    try ts.expectComment("bar");
    try ts.expectPiStart("baz");
    try ts.expectNull();
}

test "TokenStream Element & Attributes" {
    var ts = TestTokenStream{};

    ts.reset("<foo bar = 'baz'/>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValText("baz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset("<foo bar = '&baz;'/>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValEntref("baz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset("<foo bar = 'fizz&baz;buzz'/>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValText("fizz");
    try ts.expectAttrValEntref("baz");
    try ts.expectAttrValText("buzz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset("<foo bar = '&fizz;baz&buzz;'/>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValEntref("fizz");
    try ts.expectAttrValText("baz");
    try ts.expectAttrValEntref("buzz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset("<foo bar = '&fizz;&baz;&buzz;'/>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValEntref("fizz");
    try ts.expectAttrValEntref("baz");
    try ts.expectAttrValEntref("buzz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset("<foo bar='baz'><fizz buzz = \"boo\" /></foo>").unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectAttrName("bar");
    try ts.expectAttrValText("baz");
    try ts.expectElemOpen("fizz");
    try ts.expectAttrName("buzz");
    try ts.expectAttrValText("boo");
    try ts.expectElemClose("fizz");
    try ts.expectElemClose("foo");
    try ts.expectNull();

    ts.reset(
        \\<foo>
        \\    <bar>
        \\        <baz fizz= "buzz" />
        \\    </bar>
        \\</foo>
    ).unwrap() catch unreachable;
    try ts.expectElemOpen("foo");
    try ts.expectWhitespace("\n    ");
    try ts.expectElemOpen("bar");
    try ts.expectWhitespace("\n        ");
    try ts.expectElemOpen("baz");
    try ts.expectAttrName("fizz");
    try ts.expectAttrValText("buzz");
    try ts.expectElemClose("baz");
    try ts.expectWhitespace("\n    ");
    try ts.expectElemClose("bar");
    try ts.expectWhitespace("\n");
    try ts.expectElemClose("foo");
    try ts.expectNull();
}
