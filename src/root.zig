const std = @import("std");
pub const regex = @import("regex.zig");

comptime {
    std.testing.refAllDeclsRecursive(regex);
}
