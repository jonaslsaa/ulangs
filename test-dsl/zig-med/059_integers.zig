
const print = @import("std").debug.print;

pub fn main() void {
    const zig = [_]u8{
        0o132, // octal
        0b1101001, // binary
        0x67, // hex
    };

    print("{s} is cool.\n", .{zig});
}
