const std = @import("std");

fn add(T: type, a: T, b: T) T {
    return a + b;
}

pub fn main() void {
    const result = add(i32, 10, 20);
    std.debug.print("Sum: {}\n", .{result});
}
