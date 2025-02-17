const print = @import("std").debug.print;

pub fn main() void {
    const n = 5;
    var foo_frame = async foo(n);

    resume foo_frame;
    resume foo_frame;
    resume foo_frame;
    resume foo_frame;
    resume foo_frame;

    print("\n", .{});
}

fn foo(countdown: u32) void {
    var current = countdown;

    while (current > 0) {
        print("{} ", .{current});
        current -= 1;
        suspend {}
    }
}
