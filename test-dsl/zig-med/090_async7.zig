const print = @import("std").debug.print;

pub fn main() void {

    var my_beef = nosuspend getBeef(0);

    print("beef? {X}!\n", .{my_beef});
}

fn getBeef(input: u32) u32 {
    if (input == 0xDEAD) {
        suspend {}
    }

    return 0xBEEF;
}
