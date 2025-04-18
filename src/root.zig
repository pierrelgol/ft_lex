const std = @import("std");
pub const Parser = @import("regex/Parser.zig");

comptime {
    std.testing.refAllDecls(Parser);
}
