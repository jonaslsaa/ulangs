const std = @import("std");

pub fn main() void {
    const a: u8 = 12;
    const b: *const u8 = &a; // fix this!

    std.debug.print("a: {}, b: {}\n", .{ a, b.* });
}
