const print = @import("std").debug.print;
const sentinel = @import("std").meta.sentinel;

pub fn main() void {
    var nums = [_:0]u32{ 1, 2, 3, 4, 5, 6 };

    const ptr: [*:0]u32 = &nums;

    nums[3] = 0;

    printSequence(nums);
    printSequence(ptr);

    print("\n", .{});
}

fn printSequence(my_seq: anytype) void {
    const my_typeinfo = @typeInfo(@TypeOf(my_seq));

    switch (my_typeinfo) {
        .array => {
            print("Array:", .{});

            for (my_seq) |s| {
                print("{}", .{s});
            }
        },
        .pointer => {
            const my_sentinel = sentinel(@TypeOf(my_seq));
            print("Many-item pointer:", .{});

            var i: usize = 0;
            while (my_seq[i] != my_sentinel) {
                print("{}", .{my_seq[i]});
                i += 1;
            }
        },
        else => unreachable,
    }
    print(". ", .{});
}
