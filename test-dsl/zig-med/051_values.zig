const std = @import("std");


const Character = struct {
    gold: u32 = 0,
    health: u8 = 100,
    experience: u32 = 0,
};


const the_narrator = Character{
    .gold = 12,
    .health = 99,
    .experience = 9000,
};


var global_wizard = Character{};


pub fn main() void {


    var glorp = Character{
        .gold = 30,
    };


    const reward_xp: u32 = 200;


    const print = std.debug.print;


    var glorp_access1: Character = glorp;
    glorp_access1.gold = 111;
    print("1:{}!. ", .{glorp.gold == glorp_access1.gold});


    var glorp_access2: *Character = &glorp;
    glorp_access2.gold = 222;
    print("2:{}!. ", .{glorp.gold == glorp_access2.gold});


    const glorp_access3: *Character = &glorp;
    glorp_access3.gold = 333;
    print("3:{}!. ", .{glorp.gold == glorp_access3.gold});

    print("XP before:{}, ", .{glorp.experience});

    levelUp(&glorp, reward_xp);

    print("after:{}.\n", .{glorp.experience});
}

fn levelUp(character_access: *Character, xp: u32) void {
    character_access.experience += xp;
}

