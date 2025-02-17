const std = @import("std");

pub fn main() void {
    defer std.debug.print("Two\n", .{});
    std.debug.print("One ", .{});
}
