const std = @import("std");
const unicode = std.unicode;

pub fn validateUtf8Slice(src: []const u8) ValidateSliceResult {
    var i: usize = 0;
    while (i < src.len) {
        const cp_len = unicode.utf8ByteSequenceLength(src[i]) catch |err| return @unionInit(ValidateSliceResult, "err", ValidateSliceResult.ErrContext{
            .src = src,
            .code = err,
            .index = i,
        });
        if (i + cp_len > src.len) return @unionInit(ValidateSliceResult, "err", ValidateSliceResult.ErrContext{
            .src = src,
            .code = ValidateSliceResult.Error.Utf8ByteSequenceLengthTooLong,
            .index = i,
        });
        if (unicode.utf8Decode(src[i .. i + cp_len])) |_| {} else |err| return @unionInit(ValidateSliceResult, "err", ValidateSliceResult.ErrContext{
            .src = src,
            .code = err,
            .index = i,
        });
        i += cp_len;
    }
    return .ok;
}

pub const ValidateSliceResult = union(enum) {
    const Self = @This();
    ok,
    err: ErrContext,

    pub const ErrContext = struct {
        src: []const u8,
        code: ValidateSliceResult.Error,
        index: usize,
    };

    pub fn unwrap(result: Self) Self.Error!void {
        return switch (result) {
            .ok => {},
            .err => |err| err.code,
        };
    }

    pub const Error = error{
        Utf8ByteSequenceLengthTooLong,

        Utf8InvalidStartByte,
        Utf8ExpectedContinuation,
        Utf8OverlongEncoding,
        Utf8EncodesSurrogateHalf,
        Utf8CodepointTooLarge,
    };
};
