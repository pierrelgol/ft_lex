const std = @import("std");
const Parser = @import("Parser.zig");

comptime {
    std.testing.refAllDeclsRecursive(Parser);
}
