const std = @import("std");
const print = std.debug.print;

const Role = enum {
    wizard,
    thief,
    bard,
    warrior,
};

pub fn main() void {
    const roles = [4]Role{ .wizard, .bard, .bard, .warrior };
    const gold = [4]u16{ 25, 11, 5, 7392 };
    const experience = [4]u8{ 40, 17, 55, 21 };

    for (roles, gold, experience, 1..) |c, g, e, i| {
        const role_name = switch (c) {
            .wizard => "Wizard",
            .thief => "Thief",
            .bard => "Bard",
            .warrior => "Warrior",
        };

        std.debug.print("{d}. {s} (Gold: {d}, XP: {d})\n", .{
            i,
            role_name,
            g,
            e,
        });
    }
}
