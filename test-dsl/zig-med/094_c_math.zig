
const std = @import("std");

const c = @cImport({
    @cInclude("math.h");
});

pub fn main() !void {
    const angle = 765.2;
    const circle = 360;

    const result = c.fmod(angle, circle);

    std.debug.print("The normalized angle of {d: >3.1} degrees is {d: >3.1} degrees.\n", .{ angle, result });
}
