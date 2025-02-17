const print = @import("std").debug.print;

pub fn main() void {
    const instructions = "+3 *5 -2 *2";

    var value: u32 = 0;

    comptime var i = 0;

    inline while (i < instructions.len) : (i += 3) {

        const digit = instructions[i + 1] - '0';

        switch (instructions[i]) {
            '+' => value += digit,
            '-' => value -= digit,
            '*' => value *= digit,
            else => unreachable,
        }
    }

    print("{}\n", .{value});
}
