const print = @import("std").debug.print;

fn Circle(comptime T: type) type {
    return struct {
        center_x: T,
        center_y: T,
        radius: T,
    };
}

pub fn main() void {
    const circle1 = Circle(i32){
        .center_x = 25,
        .center_y = 70,
        .radius = 15,
    };

    const circle2 = Circle(f32){
        .center_x = 25.234,
        .center_y = 70.999,
        .radius = 15.714,
    };

    print("[{s}: {},{},{}] ", .{
        stripFname(@typeName(@TypeOf(circle1))),
        circle1.center_x,
        circle1.center_y,
        circle1.radius,
    });

    print("[{s}: {d:.1},{d:.1},{d:.1}]\n", .{
        stripFname(@typeName(@TypeOf(circle2))),
        circle2.center_x,
        circle2.center_y,
        circle2.radius,
    });
}

fn stripFname(mytype: []const u8) []const u8 {
    return mytype[22..];
}
