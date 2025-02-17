const std = @import("std");
const print = std.debug.print;

pub fn main() !void {

    const poem =
        \\My name is Ozymandias, King of Kings;
        \\Look on my Works, ye Mighty, and despair!
    ;

    var it = std.mem.tokenizeAny(u8, poem, " ,;!\n");

    var cnt: usize = 0;
    while (it.next()) |word| {
        cnt += 1;
        print("{s}\n", .{word});
    }

    print("This little poem has {d} words!\n", .{cnt});
}
