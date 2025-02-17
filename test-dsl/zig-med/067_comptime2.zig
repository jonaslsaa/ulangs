const print = @import("std").debug.print;

pub fn main() void {
    comptime var count = 0;

    count += 1;
    const a1: [count]u8 = .{'A'} ** count;

    count += 1;
    const a2: [count]u8 = .{'B'} ** count;

    count += 1;
    const a3: [count]u8 = .{'C'} ** count;

    count += 1;
    const a4: [count]u8 = .{'D'} ** count;

    print("{s} {s} {s} {s}\n", .{ a1, a2, a3, a4 });

}
