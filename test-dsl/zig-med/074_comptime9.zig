const print = @import("std").debug.print;

const llama_count = 5;

const llamas = makeLlamas(llama_count);

fn makeLlamas(comptime count: usize) [count]u8 {
    var temp: [count]u8 = undefined;
    var i = 0;

    while (i < count) : (i += 1) {
        temp[i] = i;
    }

    return temp;
}

pub fn main() void {
    print("My llama value is {}.\n", .{llamas[2]});
}
