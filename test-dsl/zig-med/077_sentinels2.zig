const print = @import("std").debug.print;

const WeirdContainer = struct {
    data: [*]const u8,
    length: usize,
};

pub fn main() void {
    const foo = WeirdContainer{
        .data = "Weird Data!",
        .length = 11,
    };

    const printable = foo.data[0..foo.length];

    print("{s}\n", .{printable});
}
