const std = @import("std");
pub const Parser = @import("Parser.zig");
pub const Glushkov = @import("Glushkov.zig");

comptime {
    std.testing.refAllDeclsRecursive(Parser);
    std.testing.refAllDeclsRecursive(Glushkov);
}
