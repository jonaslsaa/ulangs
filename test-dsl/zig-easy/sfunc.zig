const std = @import("std");

fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn main() void {
    const result = add(5, 7);
    std.debug.print("Sum: {}\n", .{result});
}
