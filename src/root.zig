const std = @import("std");
const regex = @import("regex.zig");

comptime {
    std.testing.refAllDecls(regex);
}
