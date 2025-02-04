const std = @import("std");

pub fn main() void {
    var arr = [_]u8{1, 2, 3, 4, 5};
    const slice = arr[1..4];

    std.debug.print("Slice: ", .{});
    for (slice) |item| {
        std.debug.print("{} ", .{item});
    }
    std.debug.print("\n", .{});
}
