const std = @import("std");

pub fn main() void {
    const value = 42;

    if (value < 50) {
        std.debug.print("Value is less than 50.\n", .{});
    } else {
        std.debug.print("Value is 50 or greater.\n", .{});
    }
}
