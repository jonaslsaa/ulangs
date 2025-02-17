
const print = @import("std").debug.print;

pub fn main() void {
    var letter: u8 = 'A';

    const my_letter: ?*[1]u8 = &letter;

    print("Letter: {u}\n", .{my_letter.?.*[0]});
}
