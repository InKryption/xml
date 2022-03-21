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
        const start = tok.info.elem_close.name_index;
        const end = start + tok.info.elem_close.name_len;
        return src[start..end];
    }

    pub fn len(tok: Tok) usize {
        return switch (tok.info) {
            .err => unreachable,
            .whitespace => |whitespace| whitespace.len,
            .pi_start => |pi_start| pi_start.len,
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

    const first_elem_open_len: ?usize = prolog_tokenization: while (true) {
        i = utility.nextNonXmlWhitespaceCharIndexAfter(src, i);
        tag_tokenizer.resetUnchecked(src[i..]);

        if (tag_tokenizer.next()) |start_tag_tok| {
            switch (start_tag_tok.info) {
                .pi_open => {
                    tokenize_pi: {
                        const pi_target = tag_tokenizer.next() orelse std.debug.todo("Error here.");
                        if (pi_target.info != .pi_target) std.debug.todo("Error here.");

                        const len: usize = pi_target.index + pi_target.info.pi_target.len;
                        suspend ts.emitResult(i, .pi_start, .{ .len = len });

                        break :tokenize_pi;
                    }

                    while (tag_tokenizer.next()) |pi_tok_str_end| {
                        switch (pi_tok_str_end.info) {
                            .pi_tok => |pi_tok| {
                                const len: usize = pi_tok.len;
                                suspend ts.emitResult(i + pi_tok_str_end.index, .pi_tok, .{ .len = len });
                            },
                            .pi_str => |pi_str| {
                                const len: usize = pi_str.len;
                                suspend ts.emitResult(i + pi_tok_str_end.index, .pi_str, .{ .len = len });
                            },
                            .pi_close => {
                                i += pi_tok_str_end.index + pi_tok_str_end.info.cannonicalSlice().?.len;
                                continue :prolog_tokenization;
                            },
                            else => std.debug.todo("Error here."),
                        }
                    } else std.debug.todo("Error here.");
                },
                .pi_target,
                .pi_tok,
                .pi_str,
                .pi_close,
                => unreachable,

                .comment_start => {
                    const comment_text = tag_tokenizer.next() orelse std.debug.todo("Error here.");
                    if (comment_text.info != .comment_text) std.debug.todo("Error here.");

                    const comment_end = tag_tokenizer.next() orelse std.debug.todo("Error here.");
                    if (comment_end.info != .comment_end) std.debug.todo("Error here.");

                    const len: usize = comment_end.index + comment_end.info.cannonicalSlice().?.len;
                    suspend ts.emitResult(i, .comment, .{ .len = len });
                    i += len;
                },
                .comment_text,
                .comment_end,
                => unreachable,

                .cdata_start => std.debug.todo("Error here."),
                .cdata_text,
                .cdata_end,
                => unreachable,

                .elem_open_start => {
                    const elem_tag_name = tag_tokenizer.next() orelse std.debug.todo("Error here.");
                    if (elem_tag_name.info != .elem_tag_name) std.debug.todo("Error here.");

                    const len: usize = elem_tag_name.index + elem_tag_name.info.elem_tag_name.len;
                    break :prolog_tokenization @as(?usize, len);
                },
                .elem_close_start => std.debug.todo("Error here."),
                .elem_close_inline => unreachable,
                .elem_tag_end => unreachable,
                .elem_tag_name => unreachable,

                else => std.debug.todo("Do the rest"),
            }
        } else break :prolog_tokenization @as(?usize, null);
    } else unreachable;

    var depth: usize = if (first_elem_open_len) |tok_len| depth: {
        suspend ts.emitResult(i, .elem_open, .{ .len = tok_len });
        i += tok_len;
        break :depth 1;
    } else 0;
    body_tokenization: while (depth != 0) {
        continue :body_tokenization;
    }

    while (true) {
        suspend {}
    }
}

fn emitResult(ts: *TokenStream, index: usize, comptime id: Tok.Id, expr: anytype) void {
    std.debug.assert(ts.tok.* == null);
    ts.tok.* = Tok.init(index, id, expr);
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

test "TokenStream Basic Usages" {
    var ts = TestTokenStream{};

    ts.reset("<!-- foo --><?foo bar 'baz'?><!-- fizz -->").unwrap() catch unreachable;
    try ts.expectComment(" foo ");
    try ts.expectPiStart("foo");
    try ts.expectPiTok("bar");
    try ts.expectPiStr("baz");
    try ts.expectComment(" fizz ");
    try ts.expectNull();
}
