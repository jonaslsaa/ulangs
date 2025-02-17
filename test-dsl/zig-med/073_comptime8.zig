const print = @import("std").debug.print;

const llama_count = 5;
const llamas = [llama_count]u32{ 5, 10, 15, 20, 25 };

pub fn main() void {
    const my_llama = getLlama(4);

    print("My llama value is {}.\n", .{my_llama});
}

fn getLlama(comptime i: usize) u32 {
    comptime assert(i < llama_count);

    return llamas[i];
}

fn assert(ok: bool) void {
    if (!ok) unreachable;
}
