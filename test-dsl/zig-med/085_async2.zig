const print = @import("std").debug.print;

pub fn main() void {
    var foo_frame = async foo();
    resume foo_frame;
}

fn foo() void {
    print("Hello ", .{});
    suspend {}
    print("async!\n", .{});
}
