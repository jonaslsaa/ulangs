const print = @import("std").debug.print;

pub fn main() void {
    const data: [*]const u8 = "Weird Data!";

    const printable: [*:0]const u8 = @ptrCast(data);

    print("{s}\n", .{printable});
}
