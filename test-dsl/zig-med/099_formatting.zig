const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    const size = 15;

    print("\n X |", .{});

    for (0..size) |n| {
        print("{d:>3} ", .{n + 1});
    }
    print("\n", .{});

    var n: u8 = 0;
    while (n <= size) : (n += 1) {
        print("---+", .{});
    }
    print("\n", .{});

    for (0..size) |a| {
        print("{d:>2} |", .{a + 1});

        for (0..size) |b| {
            print("{d:>3} ", .{(a + 1) * (b + 1)});
        }

        print("\n\n", .{});
    }
}
