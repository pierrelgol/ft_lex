const std = @import("std");

pub const Lexer = @import("regex/Lexer.zig");
pub const Token = @import("regex/Token.zig").Token;
pub const TokenResult = @import("regex/Token.zig").TokenResult;
pub const RegexEre = @import("regex/ere.zig").Tokenizer;
pub const RegexBre = @import("regex/bre.zig").Tokenizer;
pub const RegexLex = @import("regex/lex.zig").Tokenizer;

test "Lexer" {
    const input: []const u8 = "ab|cd";

    {
        var ere: RegexBre = .init(input);
        const lexer = ere.lexer();

        while (lexer.peek() != .end_of_expression) {
            std.debug.print("{}", .{lexer.next()});
        }
    }

    {
        var ere: RegexEre = .init(input);
        const lexer = ere.lexer();

        while (lexer.peek() != .end_of_expression) {
            std.debug.print("{}", .{lexer.next()});
        }
    }

    {
        var ere: RegexLex = .init(input);
        const lexer = ere.lexer();

        while (lexer.peek() != .end_of_expression) {
            std.debug.print("{}", .{lexer.next()});
        }
    }
}
