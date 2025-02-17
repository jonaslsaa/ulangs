const print = @import("std").debug.print;

var global_counter: i32 = 0;

pub fn main() void {
    var foo_frame = async foo();

    while (global_counter <= 5) {
        print("{} ", .{global_counter});
        resume foo_frame;
    }

    print("\n", .{});
}

fn foo() void {
    while (true) {
        global_counter += 1;
        suspend {}
    }
}
