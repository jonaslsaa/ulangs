const std = @import("std");

pub fn main() void {
    const answer: u8 = deepThought();

    std.debug.print("Answer to the Ultimate Question: {}\n", .{answer});
}

fn deepThought() u8 {
    return 42; // Number courtesy Douglas Adams
}
