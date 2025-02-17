const print = @import("std").debug.print;

pub fn main() void {
    print("A", .{});

    var frame = async suspendable();

    print("D", .{});

    resume frame;

    print("F", .{});
}

fn suspendable() void {
    print("B", .{});

    suspend {
        print("C", .{});
    }

    print("E", .{});
}
