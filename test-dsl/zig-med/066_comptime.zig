const print = @import("std").debug.print;

pub fn main() void {
    const const_int = 12345;
    const const_float = 987.654;

    print("Immutable: {}, {d:.3}; ", .{ const_int, const_float });

    var var_int: u32 = 12345;
    var var_float: f32 = 987.654;

    var_int = 54321;
    var_float = 456.789;

    print("Mutable: {}, {d:.3}; ", .{ var_int, var_float });

    print("Types: {}, {}, {}, {}\n", .{
        @TypeOf(const_int),
        @TypeOf(const_float),
        @TypeOf(var_int),
        @TypeOf(var_float),
    });
}
