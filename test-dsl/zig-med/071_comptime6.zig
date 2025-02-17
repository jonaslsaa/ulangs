const print = @import("std").debug.print;

const Narcissus = struct {
    me: *Narcissus = undefined,
    myself: *Narcissus = undefined,
    echo: void = undefined,
};

pub fn main() void {
    print("Narcissus has room in his heart for:", .{});


    const fields = @typeInfo(Narcissus).@"struct".fields;

    inline for (fields) |field| {
        if (field.type != void) {
            print(" {s}", .{field.name});
        }
    }


    print(".\n", .{});
}
