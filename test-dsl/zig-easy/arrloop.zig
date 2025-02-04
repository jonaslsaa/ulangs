const std = @import("std");

pub fn main() void {
    const arr = [_]i32{1, 2, 3, 4, 5};

    for (arr) |item| {
        std.debug.print("Item: {}\n", .{item});
    }
}
