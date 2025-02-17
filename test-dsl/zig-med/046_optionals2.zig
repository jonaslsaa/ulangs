const std = @import("std");

const Elephant = struct {
    letter: u8,
    tail: ?*Elephant = null, // Hmm... tail needs something...
    visited: bool = false,
};

pub fn main() void {
    var elephantA = Elephant{ .letter = 'A' };
    var elephantB = Elephant{ .letter = 'B' };
    var elephantC = Elephant{ .letter = 'C' };

    linkElephants(&elephantA, &elephantB);
    linkElephants(&elephantB, &elephantC);


    visitElephants(&elephantA);

    std.debug.print("\n", .{});
}

fn linkElephants(e1: ?*Elephant, e2: ?*Elephant) void {
    e1.?.tail = e2.?;
}

fn visitElephants(first_elephant: *Elephant) void {
    var e = first_elephant;

    while (!e.visited) {
        std.debug.print("Elephant {u}. ", .{e.letter});
        e.visited = true;


        e = e.tail orelse break;
    }
}
