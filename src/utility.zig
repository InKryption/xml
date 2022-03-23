const std = @import("std");
const mem = std.mem;
const debug = std.debug;
const unicode = std.unicode;

const assert = debug.assert;

pub fn codepointAt(src: []const u8, i: usize, cp_len: *u3) u21 {
    assert(i < src.len);
    const codepoint_length = unicode.utf8ByteSequenceLength(src[i]) catch unreachable;
    assert(i + codepoint_length <= src.len);
    cp_len.* = codepoint_length;
    return unicode.utf8Decode(src[i .. i + cp_len.*]) catch unreachable;
}

pub const xml = struct {
    /// Returns the index of the next non-whitespace character after the provided index, inclusive of the provided index.
    pub fn nextNonWhitespaceCharIndexAfter(src: []const u8, start: usize) usize {
        var new_index: usize = start;
        while (new_index < src.len) {
            var cp_len: u3 = undefined;
            if (xml.isWhitespaceChar(codepointAt(src, new_index, &cp_len))) {
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
            if (xml.isNameChar(codepointAt(src, new_index, &cp_len))) {
                new_index += cp_len;
            } else break;
        }
        return new_index;
    }

    /// If the codepoint at the specified index in the string is a valid XML name start character,
    /// returns the length of the codepoint; otherwise, returns null.
    /// Asserts that the specified index is less than the length of the string.
    /// Asserts that the index is not 0.
    pub fn nameStartCharLengthAt(src: []const u8, i: usize) ?u3 {
        assert(i < src.len);
        assert(i != 0);

        var cp_len: u3 = undefined;
        const cp = codepointAt(src, i, &cp_len);
        return if (xml.isNameStartChar(cp)) cp_len else null;
    }

    pub inline fn isWhitespaceChar(cp: u21) bool {
        return switch (cp) {
            ' ', '\t', '\n', '\r' => true,
            else => false,
        };
    }

    pub inline fn isNameStartChar(cp: u21) bool {
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

    pub inline fn isNameChar(cp: u21) bool {
        return xml.isNameStartChar(cp) or switch (cp) {
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
};

/// Returns the enum value of an enum set which possesses the longest name; null if the
/// enum contains no values.
pub fn longestEnumName(comptime E: type) ?E {
    const values = std.enums.values(E);
    const lessThan = struct {
        fn lessThan(_: void, lhs: E, rhs: E) bool {
            return @tagName(lhs).len < @tagName(rhs).len;
        }
    }.lessThan;
    const max_index = std.sort.argMax(E, values, void{}, lessThan) orelse return null;
    return values[max_index];
}
