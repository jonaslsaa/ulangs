const std = @import("std");

const MyNumberError = error{TooSmall};

pub fn main() void {
    var my_number: MyNumberError!u8 = 5;

    my_number = MyNumberError.TooSmall;

    std.debug.print("I compiled!\n", .{});
}
