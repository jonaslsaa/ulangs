const v1 = @Vector(3, i32){ 1, 10, 100 };
const v2 = @Vector(3, f32){ 2.0, 3.0, 5.0 };

const v3 = v1 + v1; // {   2,  20,  200};
const v4 = v2 * v2; // { 4.0, 9.0, 25.0};

const v5: @Vector(3, f32) = @floatFromInt(v3); // { 2.0,  20.0,  200.0}
const v6 = v4 - v5; // { 2.0, -11.0, -175.0}
const v7 = @abs(v6); // { 2.0,  11.0,  175.0}

const v8: @Vector(4, u8) = @splat(2); // { 2, 2, 2, 2}
const v8_sum = @reduce(.Add, v8); // 8
const v8_min = @reduce(.Min, v8); // 2

const single_digit_primes = [4]i8{ 2, 3, 5, 7 };
const prime_vector: @Vector(4, i8) = single_digit_primes;


fn calcMaxPairwiseDiffOld(list1: [4]f32, list2: [4]f32) f32 {
    var max_diff: f32 = 0;
    for (list1, list2) |n1, n2| {
        const abs_diff = @abs(n1 - n2);
        if (abs_diff > max_diff) {
            max_diff = abs_diff;
        }
    }
    return max_diff;
}


const Vec4 = @Vector(4, f32);
fn calcMaxPairwiseDiffNew(a: Vec4, b: Vec4) f32 {
    const abs_diff_vec = @abs(a - b);
    const max_diff = @reduce(.Max, abs_diff_vec);
    return max_diff;
}


const std = @import("std");
const print = std.debug.print;

pub fn main() void {
    const l1 = [4]f32{ 3.141, 2.718, 0.577, 1.000 };
    const l2 = [4]f32{ 3.154, 2.707, 0.591, 0.993 };
    const mpd_old = calcMaxPairwiseDiffOld(l1, l2);
    const mpd_new = calcMaxPairwiseDiffNew(l1, l2);
    print("Max difference (old fn): {d: >5.3}\n", .{mpd_old});
    print("Max difference (new fn): {d: >5.3}\n", .{mpd_new});
}
