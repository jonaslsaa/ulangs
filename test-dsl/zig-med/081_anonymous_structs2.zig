const print = @import("std").debug.print;

pub fn main() void {
    printCircle(.{
        .center_x = @as(u32, 205),
        .center_y = @as(u32, 187),
        .radius = @as(u32, 12),
    });
}

fn printCircle(circle: anytype) void {
    print("x:{} y:{} radius:{}\n", .{
        circle.center_x,
        circle.center_y,
        circle.radius,
    });
}
