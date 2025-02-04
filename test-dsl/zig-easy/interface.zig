const std = @import("std");

const Speaker = struct {
    pub fn speak() void {
        std.debug.print("Hello, I am speaking!\n", .{});
    }
};

pub fn main() void {
    Speaker.speak();
}
