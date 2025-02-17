const std = @import("std");

var counter: u32 = 0;

const MyErr = error{ GetFail, IncFail };

pub fn main() void {
    const a: u32 = makeNumber() catch return;
    const b: u32 = makeNumber() catch return;

    std.debug.print("Numbers: {}, {}\n", .{ a, b });
}

fn makeNumber() MyErr!u32 {
    std.debug.print("Getting number...", .{});

    errdefer std.debug.print("failed!\n", .{});

    var num = try getNumber(); // <-- This could fail!

    num = try increaseNumber(num); // <-- This could ALSO fail!

    std.debug.print("got {}. ", .{num});

    return num;
}

fn getNumber() MyErr!u32 {
    return 4;
}

fn increaseNumber(n: u32) MyErr!u32 {
    if (counter > 0) return MyErr.IncFail;

    counter += 1;

    return n + 1;
}
