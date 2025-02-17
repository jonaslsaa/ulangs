const std = @import("std");

pub fn main() void {
    const bits = [_]u8{ 1, 0, 1, 1 };
    var value: u32 = 0;

    for (bits, 0..) |bit, i| {
        const i_u32: u32 = @intCast(i);
        const place_value = std.math.pow(u32, 2, i_u32);
        value += place_value * bit;
    }

    std.debug.print("The value of bits '1101': {}.\n", .{value});
}
