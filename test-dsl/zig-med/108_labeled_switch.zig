const std = @import("std");

const PullRequestState = enum(u8) {
    Draft,
    InReview,
    Approved,
    Rejected,
    Merged,
};

pub fn main() void {
    pr: switch (PullRequestState.Draft) {
        PullRequestState.Draft => continue :pr PullRequestState.InReview,
        PullRequestState.InReview => continue :pr PullRequestState.Approved,
        PullRequestState.Approved => continue :pr PullRequestState.Merged,
        PullRequestState.Rejected => {
            std.debug.print("The pull request has been rejected.\n", .{});
            return;
        },
        PullRequestState.Merged => break :pr, // Would you know where to break to?
    }
    std.debug.print("The pull request has been merged.\n", .{});
}
