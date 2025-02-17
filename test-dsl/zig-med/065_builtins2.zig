const print = @import("std").debug.print;

const Narcissus = struct {
    me: *Narcissus = undefined,
    myself: *Narcissus = undefined,
    echo: void = undefined, // Alas, poor Echo!

    fn fetchTheMostBeautifulType() type {
        return @This();
    }
};

pub fn main() void {
    var narcissus: Narcissus = Narcissus{};

    narcissus.me = &narcissus;
    narcissus.myself = &narcissus;

    const Type1 = @TypeOf(narcissus, narcissus.me.*, narcissus.myself.*);

    const Type2 = Narcissus.fetchTheMostBeautifulType();

    print("A {s} loves all {s}es. ", .{
        maximumNarcissism(Type1),
        maximumNarcissism(Type2),
    });


    print("He has room in his heart for:", .{});

    const fields = @typeInfo(Narcissus).@"struct".fields;

    if (fields[0].type != void) {
        print(" {s}", .{fields[0].name});
    }

    if (fields[1].type != void) {
        print(" {s}", .{fields[1].name});
    }

    if (fields[2].type != void) {
        print(" {s}", .{fields[2].name});
    }


    print(".\n", .{});
}

fn maximumNarcissism(myType: anytype) []const u8 {
    const indexOf = @import("std").mem.indexOf;

    const name = @typeName(myType);
    return name[indexOf(u8, name, ".").? + 1 ..];
}
