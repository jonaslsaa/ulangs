const std = @import("std");

pub fn main() void {
    var some_primes = [_]u8{ 1, 3, 5, 7, 11, 13, 17, 19 };

    some_primes[0] = 2;

    const first = some_primes[0];

    const fourth = some_primes[3];

    const length = some_primes.len;

    std.debug.print("First: {}, Fourth: {}, Length: {}\n", .{
        first, fourth, length,
    });
}
