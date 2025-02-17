const std = @import("std");

pub fn main() void {
    var n: u32 = 2;

    while (n < 1024) {
        std.debug.print("{} ", .{n});

        n *= 2;
    }

    std.debug.print("n={}\n", .{n});
}
