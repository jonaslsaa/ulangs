const std = @import("std");

pub fn main() void {
    var cards = [8]u8{ 'A', '4', 'K', '8', '5', '2', 'Q', 'J' };

    const hand1: []u8 = cards[0..4];
    const hand2: []u8 = cards[4..];

    std.debug.print("Hand1: ", .{});
    printHand(hand1);

    std.debug.print("Hand2: ", .{});
    printHand(hand2);
}

fn printHand(hand: []u8) void {
    for (hand) |h| {
        std.debug.print("{u} ", .{h});
    }
    std.debug.print("\n", .{});
}
