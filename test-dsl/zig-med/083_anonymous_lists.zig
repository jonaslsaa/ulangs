const print = @import("std").debug.print;

pub fn main() void {
    const hello: [5]u8 = .{ 'h', 'e', 'l', 'l', 'o' };
    print("I say {s}!\n", .{hello});
}
