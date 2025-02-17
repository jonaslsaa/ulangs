const std = @import("std");

pub fn main() void {

    for (1..21) |n| {

        if (n % 3 == 0) continue;
        if (n % 5 == 0) continue;
        std.debug.print("{} ", .{n});
    }

    std.debug.print("\n", .{});
}
