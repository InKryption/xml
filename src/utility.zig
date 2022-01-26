const std = @import("std");
const mem = std.mem;
const debug = std.debug;
const unicode = std.unicode;

const assert = debug.assert;

pub fn unexpectedEof(src: []const u8, i: usize) bool {
    assert(i <= src.len);
    return i == src.len;
}

pub fn assertEqualStrings(a: []const u8, b: []const u8) void {
    assert(mem.eql(u8, a, b));
}

pub fn codepointAt(src: []const u8, i: usize, cp_len: *u3) u21 {
    assert(i < src.len);
    const codepoint_length = unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
    assert(i + codepoint_length <= src.len);
    cp_len.* = codepoint_length;
    return unicode.utf8Decode(src[i .. i + cp_len.*]) catch unreachable;
}

/// Returns the index of the next non-whitespace character after the provided index, inclusive of the provided index.
pub fn nextNonWhitespaceCharIndexAfter(src: []const u8, start: usize) usize {
    var new_index: usize = start;
    while (new_index < src.len) {
        var cp_len: u3 = undefined;
        if (isWhitespaceChar(codepointAt(src, new_index, &cp_len))) {
            new_index += cp_len;
        } else break;
    }
    return new_index;
}

/// Returns the index of the next non-name character after the provided index, inclusive of the provided index.
pub fn nextNonNameCharIndexAfter(src: []const u8, start: usize) usize {
    var new_index: usize = start;
    while (new_index < src.len) {
        var cp_len: u3 = undefined;
        if (isValidNameChar(codepointAt(src, new_index, &cp_len))) {
            new_index += cp_len;
        } else break;
    }
    return new_index;
}

/// If the codepoint at the specified index in the string is a valid name start character,
/// returns the length of the codepoint; otherwise, returns null.
/// Asserts that the specified index is less than the length of the string.
/// Asserts that the index is not 0.
pub fn validNameStartCharLengthAt(src: []const u8, i: usize) ?u3 {
    assert(i < src.len);
    assert(i != 0);

    var cp_len: u3 = undefined;
    const cp = codepointAt(src, i, &cp_len);
    return if (isValidNameStartChar(cp)) cp_len else null;
}

pub inline fn isWhitespaceChar(cp: u21) bool {
    return switch (cp) {
        ' ', '\t', '\n', '\r' => true,
        else => false,
    };
}

pub inline fn isValidNameStartChar(cp: u21) bool {
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

pub inline fn isValidNameChar(cp: u21) bool {
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
