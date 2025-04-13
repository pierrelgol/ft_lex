const std = @import("std");
const Lexer = @import("Lexer.zig");
const BackendEre = @import("backends/ere.zig").Tokenizer;
const BackendBre = @import("backends/bre.zig").Tokenizer;
const BackendLex = @import("backends/lex.zig").Tokenizer;

// comptime {
//     std.testing.refAllDecls(BackendEre);
//     std.testing.refAllDecls(BackendBre);
//     std.testing.refAllDecls(BackendLex);
//     std.testing.refAllDecls(Lexer);
// }

test "Lexer" {
    const input: []const u8 = "ab|cd";
    var ere: BackendBre = .init(input);
    const lexer = ere.lexer();

    while (lexer.peek() != .end_of_expression) {
        std.debug.print("{}", .{lexer.next()});
    }
}
