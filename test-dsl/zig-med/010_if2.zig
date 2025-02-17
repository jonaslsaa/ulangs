const std = @import("std");

pub fn main() void {
    const discount = true;

    const price: u8 = if (discount) 17 else 20;

    std.debug.print("With the discount, the price is ${}.\n", .{price});
}
