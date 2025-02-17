const print = @import("std").debug.print;

pub fn main() void {
    const a: u4 = 0b1101;
    const b: u4 = 0b0101;
    const my_result = @addWithOverflow(a, b);

    print("{b:0>4} + {b:0>4} = {b:0>4} ({s})", .{ a, b, my_result[0], if (my_result[1] == 1) "true" else "false" });

    const expected_result: u8 = 0b00010010;
    print(". Without overflow: {b:0>8}. ", .{expected_result});

    print("Furthermore, ", .{});

    const input: u8 = 0b11110000;
    const tupni: u8 = @bitReverse(input);
    print("{b:0>8} backwards is {b:0>8}.\n", .{ input, tupni });
}
