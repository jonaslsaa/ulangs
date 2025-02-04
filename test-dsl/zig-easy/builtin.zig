const std = @import("std");

pub fn main() void {
    const max: i32 = @max(10, 42);
    std.debug.print("Maximum value: {}\n", .{max});
}
