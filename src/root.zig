const std = @import("std");
const regex = @import("regex.zig");

comptime {
    @setEvalBranchQuota(200000);
    std.testing.refAllDeclsRecursive(regex);
}
