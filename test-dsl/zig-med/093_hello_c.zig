
const std = @import("std");

const c = @cImport({
    @cInclude("unistd.h");
});

pub fn main() void {

    const c_res = c.write(2, "Hello C from Zig!", 17);

    std.debug.print(" - C result is {d} chars written.\n", .{c_res});
}
