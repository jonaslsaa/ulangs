const std = @import("std");

pub fn main() !void {
    std.debug.print("Starting work...\n", .{});

    {
        const handle = try std.Thread.spawn(.{}, thread_function, .{1});

        defer handle.join();

        const handle2 = try std.Thread.spawn(.{}, thread_function, .{2});
        defer handle2.join();

        const handle3 = try std.Thread.spawn(.{}, thread_function, .{3});
        defer handle3.join();

        std.time.sleep(1500 * std.time.ns_per_ms);
        std.debug.print("Some weird stuff, after starting the threads.\n", .{});
    }
    std.debug.print("Zig is cool!\n", .{});
}

fn thread_function(num: usize) !void {
    std.time.sleep(200 * num * std.time.ns_per_ms);
    std.debug.print("thread {d}: {s}\n", .{ num, "started." });

    const work_time = 3 * ((5 - num % 3) - 2);
    std.time.sleep(work_time * std.time.ns_per_s);

    std.debug.print("thread {d}: {s}\n", .{ num, "finished." });
}
