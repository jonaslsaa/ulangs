const std = @import("std");

pub fn main() void {
    var n: u32 = 1;

    while (n <= 20) : (n += 1) {
        if (n % 3 == 0) continue;
        if (n % 5 == 0) continue;
        std.debug.print("{} ", .{n});
    }

    std.debug.print("\n", .{});
}
