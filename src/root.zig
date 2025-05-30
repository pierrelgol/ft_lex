const std = @import("std");
pub const regex = @import("regex.zig");
pub const Gloushkov = @import("Gloushkov.zig");

comptime {
    std.testing.refAllDeclsRecursive(regex);
    std.testing.refAllDeclsRecursive(Gloushkov);
}
