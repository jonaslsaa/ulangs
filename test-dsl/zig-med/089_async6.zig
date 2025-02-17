const print = @import("std").debug.print;

pub fn main() void {
    var com_frame = async getPageTitle("http://example.com");
    var org_frame = async getPageTitle("http://example.org");

    var com_title = await com_frame;
    var org_title = await org_frame;

    print(".com: {s}, .org: {s}.\n", .{ com_title, org_title });
}

fn getPageTitle(url: []const u8) []const u8 {
    _ = url;
    return "Example Title";
}
