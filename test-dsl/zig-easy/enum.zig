const std = @import("std");

const Color = enum {
    Red,
    Green,
    Blue,
};

pub fn main() void {
    const favorite = Color.Green;

    std.debug.print("Favorite color is: {}\n", .{favorite});
}
