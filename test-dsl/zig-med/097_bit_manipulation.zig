
const std = @import("std");
const print = std.debug.print;

pub fn main() !void {

    var x: u8 = 1;
    var y: u8 = 0;

    x ^= y;
    y ^= x;

    x ^= y;

    print("x = {d}; y = {d}\n", .{ x, y });
}

