const std = @import("std");

pub fn main() void {
    const maybeValue: ?i32 = null;

    if (maybeValue) |value| {
        std.debug.print("Value is: {}\n", .{value});
    } else {
        std.debug.print("No value present.\n", .{});
    }
}
