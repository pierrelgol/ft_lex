const std = @import("std");
pub const regex = @import("regex.zig");
pub const nfa = @import("nfa.zig");

comptime {
    std.testing.refAllDeclsRecursive(regex);
    std.testing.refAllDeclsRecursive(nfa);
}
