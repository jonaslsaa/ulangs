const std = @import("std");

const NumError = error{IllegalNumber};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const my_num: u32 = try getNumber();

    try stdout.print("my_num={}\n", .{my_num});
}

fn getNumber() NumError!u32 {
    if (false) return NumError.IllegalNumber;
    return 42;
}
