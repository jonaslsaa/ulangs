
const std = @import("std");

pub fn main() !void {
    const cwd = std.fs.cwd();

    var output_dir = try cwd.openDir("output", .{});
    defer output_dir.close();

    const file = try output_dir.openFile("zigling.txt", .{});
    defer file.close();

    var content = [_]u8{'A'} ** 64;
    std.debug.print("{s}\n", .{content});

    const bytes_read = try file.read(&content);

    std.debug.print("Successfully Read {d} bytes: {s}\n", .{
        bytes_read,
        content[0..bytes_read], // change this line only
    });
}
