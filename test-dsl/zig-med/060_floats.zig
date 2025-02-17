
const print = @import("std").debug.print;

pub fn main() void {
    const shuttle_weight: f32 = 0.453592 * 4.480e6;

    print("Shuttle liftoff weight: {d:.0}kg\n", .{shuttle_weight});
}

