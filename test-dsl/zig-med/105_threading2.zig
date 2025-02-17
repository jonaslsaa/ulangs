const std = @import("std");

pub fn main() !void {
    const count = 1_000_000_000;
    var pi_plus: f64 = 0;
    var pi_minus: f64 = 0;

    {
        const handle1 = try std.Thread.spawn(.{}, thread_pi, .{ &pi_plus, 5, count });
        defer handle1.join();

        const handle2 = try std.Thread.spawn(.{}, thread_pi, .{ &pi_minus, 3, count });
        defer handle2.join();
    }
    std.debug.print("PI ≈ {d:.8}\n", .{4 + pi_plus - pi_minus});
}

fn thread_pi(pi: *f64, begin: u64, end: u64) !void {
    var n: u64 = begin;
    while (n < end) : (n += 4) {
        pi.* += 4 / @as(f64, @floatFromInt(n));
    }
}
