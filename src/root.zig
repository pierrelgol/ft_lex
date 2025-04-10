const std = @import("std");
pub const Lexer = @import("Lexer.zig").Lexer;
pub const Kind = @import("token.zig").Kind;
pub const Token = @import("token.zig").Token;

comptime {
    std.testing.refAllDeclsRecursive(Lexer);
    std.testing.refAllDeclsRecursive(Token);
    std.testing.refAllDeclsRecursive(Kind);
}
