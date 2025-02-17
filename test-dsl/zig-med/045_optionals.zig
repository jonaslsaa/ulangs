const std = @import("std");

pub fn main() void {
    const result = deepThought();

    const answer: u8 = result orelse 42;

    std.debug.print("The Ultimate Answer: {}.\n", .{answer});
}

fn deepThought() ?u8 {
    return null;
}
