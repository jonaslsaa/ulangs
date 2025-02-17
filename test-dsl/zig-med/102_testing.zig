const std = @import("std");
const testing = std.testing;

fn add(a: f16, b: f16) f16 {
    return a + b;
}

test "add" {

    try testing.expect(add(41, 1) == 42);

    try testing.expectEqual(add(41, 1), 42);

    try testing.expect(add(5, -4) == 1);

    try testing.expect(add(1.5, 1.5) == 3);
}

fn sub(a: f16, b: f16) f16 {
    return a - b;
}

test "sub" {
    try testing.expect(sub(10, 5) == 5);

    try testing.expect(sub(3, 1.5) == 1.5);
}

fn divide(a: f16, b: f16) !f16 {
    if (b == 0) return error.DivisionByZero;
    return a / b;
}

test "divide" {
    try testing.expect(divide(2, 2) catch unreachable == 1);
    try testing.expect(divide(-1, -1) catch unreachable == 1);
    try testing.expect(divide(10, 2) catch unreachable == 5);
    try testing.expect(divide(1, 3) catch unreachable == 0.3333333333333333);

    try testing.expectError(error.DivisionByZero, divide(15, 0));
}
