const MyNumberError = error{
    TooBig,
    TooSmall,
};

const std = @import("std");

pub fn main() void {
    const nums = [_]u8{ 2, 3, 4, 5, 6 };

    for (nums) |num| {
        std.debug.print("{}", .{num});

        const n = numberMaybeFail(num);
        if (n) |value| {
            std.debug.print("={}. ", .{value});
        } else |err| switch (err) {
            MyNumberError.TooBig => std.debug.print(">4. ", .{}),
            MyNumberError.TooSmall => std.debug.print("<4. ", .{}),
        }
    }

    std.debug.print("\n", .{});
}

fn numberMaybeFail(n: u8) MyNumberError!u8 {
    if (n > 4) return MyNumberError.TooBig;
    if (n < 4) return MyNumberError.TooSmall;
    return n;
}
