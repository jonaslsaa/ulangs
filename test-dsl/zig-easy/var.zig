const std = @import("std");

pub fn main() void {
    var x: i32 = 10;    // Mutable variable
    const y: i32 = 20;  // Immutable constant

    x += y;
    std.debug.print("x: {}\n", .{x});
}
