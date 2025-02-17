const std = @import("std");

const Err = error{Cthulhu};

pub fn main() void {
    var first_line1: *const [16]u8 = undefined;
    first_line1 = "That is not dead";

    var first_line2: Err!*const [21]u8 = Err.Cthulhu;
    first_line2 = "which can eternal lie";

    std.debug.print("{s} {!s} / ", .{ first_line1, first_line2 });

    printSecondLine();
}

fn printSecondLine() void {
    var second_line2: ?*const [18]u8 = null;
    second_line2 = "even death may die";

    std.debug.print("And with strange aeons {s}.\n", .{second_line2.?});
}
