const std = @import("std");

fn compileTimeMessage() void {
    comptime {
        std.debug.print("This message is evaluated at compile-time.\n", .{});
    }
}

pub fn main() void {
    compileTimeMessage();
}
