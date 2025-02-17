const std = @import("std");

pub fn main() !void {
    const cwd: std.fs.Dir = std.fs.cwd();

    cwd.makeDir("output") catch |e| switch (e) {
        error.PathAlreadyExists => {},
        else => return e,
    };

    var output_dir: std.fs.Dir = try cwd.openDir("output", .{});
    defer output_dir.close();

    const file: std.fs.File = try output_dir.createFile("zigling.txt", .{});
    defer file.close();

    const byte_written = try file.write("It's zigling time!");
    std.debug.print("Successfully wrote {d} bytes.\n", .{byte_written});
}
