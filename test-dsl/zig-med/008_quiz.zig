const std = @import("std");

pub fn main() void {
    const letters = "YZhifg";

    var x: usize = 1;

    var lang: [3]u8 = undefined;

    lang[0] = letters[x];

    x = 3;
    lang[1] = letters[x];

    x = 5;
    lang[2] = letters[x];

    std.debug.print("Program in {s}!\n", .{lang});
}
