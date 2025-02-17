const std = @import("std");

const Insect = union {
    flowers_visited: u16,
    still_alive: bool,
};

const AntOrBee = enum { a, b };

pub fn main() void {
    const ant = Insect{ .still_alive = true };
    const bee = Insect{ .flowers_visited = 15 };

    std.debug.print("Insect report! ", .{});

    printInsect(ant, AntOrBee.a);
    printInsect(bee, AntOrBee.b);

    std.debug.print("\n", .{});
}

fn printInsect(insect: Insect, what_it_is: AntOrBee) void {
    switch (what_it_is) {
        .a => std.debug.print("Ant alive is: {}. ", .{insect.still_alive}),
        .b => std.debug.print("Bee visited {} flowers. ", .{insect.flowers_visited}),
    }
}
