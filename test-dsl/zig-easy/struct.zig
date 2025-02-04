const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,

    pub fn print(self: Point) void {
        std.debug.print("Point({}, {})\n", .{self.x, self.y});
    }
};

pub fn main() void {
    const p = Point{ .x = 10, .y = 20 };
    p.print();
}
