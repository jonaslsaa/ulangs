const print = @import("std").debug.print;

pub fn main() void {
    const foo = .{
        true,
        false,
        @as(i32, 42),
        @as(f32, 3.141592),
    };

    printTuple(foo);

    const nothing = .{};
    print("\n", nothing);
}

fn printTuple(tuple: anytype) void {
    const fields = @typeInfo(@TypeOf(tuple)).@"struct".fields;

    inline for (fields) |field| {
        print("\"{s}\"({any}):{any} ", .{
            field.name,
            field.type,
            @field(tuple, field.name),
        });
    }
}
