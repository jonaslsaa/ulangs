const std = @import("std");

pub fn main() void {
    const ziggy = "stardust";

    const d: u8 = ziggy[4];

    const laugh = "ha " ** 3;

    const major = "Major";
    const tom = "Tom";
    const major_tom = major ++ " " ++ tom;

    std.debug.print("d={u} {s}{s}\n", .{ d, laugh, major_tom });
}
