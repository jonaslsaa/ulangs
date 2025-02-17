const print = @import("std").debug.print;

pub fn main() void {
    _ = async foo();
}

fn foo() void {
    print("foo() A\n", .{});
    suspend {}
    print("foo() B\n", .{});
}
