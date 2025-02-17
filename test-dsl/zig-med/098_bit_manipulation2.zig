const std = @import("std");
const ascii = std.ascii;
const print = std.debug.print;

pub fn main() !void {
    print("Is this a pangram? {?}!\n", .{isPangram("The quick brown fox jumps over the lazy dog.")});
}

fn isPangram(str: []const u8) bool {
    if (str.len < 26) return false;

    var bits: u32 = 0;

    for (str) |c| {
        if (ascii.isASCII(c) and ascii.isAlphabetic(c)) {
            bits |= @as(u32, 1) << @truncate(ascii.toLower(c) - 'a');
        }
    }
    return bits == 0x3ffffff;
}
