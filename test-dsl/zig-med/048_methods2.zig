const std = @import("std");

const Elephant = struct {
    letter: u8,
    tail: ?*Elephant = null,
    visited: bool = false,

    pub fn getTail(self: *Elephant) *Elephant {
        return self.tail.?; // Remember, this means "orelse unreachable"
    }

    pub fn hasTail(self: *Elephant) bool {
        return (self.tail != null);
    }

    pub fn visit(self: *Elephant) void {
        self.visited = true;
    }

    pub fn print(self: *Elephant) void {
        const v: u8 = if (self.visited) 'v' else ' ';
        std.debug.print("{u}{u} ", .{ self.letter, v });
    }
};

pub fn main() void {
    var elephantA = Elephant{ .letter = 'A' };
    var elephantB = Elephant{ .letter = 'B' };
    var elephantC = Elephant{ .letter = 'C' };

    elephantA.tail = &elephantB;
    elephantB.tail = &elephantC;

    visitElephants(&elephantA);

    std.debug.print("\n", .{});
}

fn visitElephants(first_elephant: *Elephant) void {
    var e = first_elephant;

    while (true) {
        e.print();
        e.visit();

        e = if (e.hasTail()) e.getTail() else break;
    }
}

