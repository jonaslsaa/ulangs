const std = @import("std");

const CustomAllocator = struct {
    allocator: *std.mem.Allocator,

    pub fn alloc(self: *CustomAllocator, n: usize) ![]u8 {
        return self.allocator.alloc(u8, n);
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Declare as mutable.
    var innerAlloc = arena.allocator();
    var customAlloc = CustomAllocator{ .allocator = &innerAlloc };

    const memory = try customAlloc.alloc(100);
    defer customAlloc.allocator.free(memory);

    std.debug.print("Allocated 100 bytes of memory.\n", .{});
}
