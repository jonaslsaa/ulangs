const std = @import("std");

const MyNumberError = error{TooSmall};

pub fn main() void {
    const a: u32 = addTwenty(44) catch 22;
    const b: u32 = addTwenty(4) catch 22;

    std.debug.print("a={}, b={}\n", .{ a, b });
}

fn addTwenty(n: u32) MyNumberError!u32 {
    if (n < 5) {
        return MyNumberError.TooSmall;
    } else {
        return n + 20;
    }
}
