const std = @import("std");

pub fn main() void {
    var num: u8 = 1;
    var more_nums = [_]u8{ 1, 1, 1, 1 };

    makeFive(&num);
    std.debug.print("num: {}, ", .{num});

    makeFive(&more_nums[2]);

    std.debug.print("more_nums: ", .{});
    for (more_nums) |n| {
        std.debug.print("{} ", .{n});
    }

    std.debug.print("\n", .{});
}

fn makeFive(x: *u8) void {
    x.* = 5; // fix me!
}
