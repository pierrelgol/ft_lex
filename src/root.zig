const std = @import("std");
pub const Token = @import("regex/Token.zig").Token;
pub const Lexer = @import("regex/Lexer.zig").Lexer;
pub const Parser = @import("regex/Parser.zig").Parser;
// pub const NFA = @import("regex/NFA.zig").Nfa;
pub const Ast = @import("regex/Ast.zig").Ast;

comptime {
    std.testing.refAllDecls(Token);
    std.testing.refAllDecls(Lexer);
    std.testing.refAllDecls(Parser);
    std.testing.refAllDecls(Ast);
    // std.testing.refAllDecls(NFA);
}
