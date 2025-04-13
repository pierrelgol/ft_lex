const std = @import("std");
const Lexer = @import("Lexer.zig");

comptime {
    std.testing.refAllDecls(Lexer);
}
