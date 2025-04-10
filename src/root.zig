const std = @import("std");
pub const Lexer = @import("Lexer.zig").Lexer;
pub const TokenKind = @import("token.zig").Kind;
pub const Token = @import("token.zig").Token;
pub const Parser = @import("Parser.zig").Parser;
pub const Atom = @import("atom.zig").Atom;
pub const AtomKind = @import("atom.zig").Kind;

comptime {
    std.testing.refAllDeclsRecursive(Lexer);
    std.testing.refAllDeclsRecursive(Token);
    std.testing.refAllDeclsRecursive(TokenKind);
    std.testing.refAllDeclsRecursive(Atom);
    std.testing.refAllDeclsRecursive(AtomKind);
    std.testing.refAllDeclsRecursive(Parser);
}
