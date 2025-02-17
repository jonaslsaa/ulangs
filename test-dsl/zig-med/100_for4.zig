const std = @import("std");
const print = std.debug.print;

pub fn main() void {
    const hex_nums = [_]u8{ 0xb, 0x2a, 0x77 };
    const dec_nums = [_]u8{ 11, 42, 119 };

    for (hex_nums, dec_nums) |hn, dn| {
        if (hn != dn) {
            print("Uh oh! Found a mismatch: {d} vs {d}\n", .{ hn, dn });
            return;
        }
    }

    print("Arrays match!\n", .{});
}
