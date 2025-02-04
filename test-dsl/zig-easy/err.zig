const std = @import("std");

fn mightFail(success: bool) !void {
    if (!success) {
        return error.SomeError;
    }
}

pub fn main() !void {
    if (mightFail(false)) |err| {
        std.debug.print("Error occurred: {}\n", .{err});
    } else {
        std.debug.print("Operation succeeded.\n", .{});
    }
}
