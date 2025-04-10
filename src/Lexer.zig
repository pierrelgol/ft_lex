const std = @import("std");
const ascii = std.ascii;

const Iterator = @import("Iterator.zig").Iterator;
const tok = @import("token.zig");
const TokenKind = tok.Kind;
const Token = tok.Token;

pub const Lexer = struct {
    tokens: [MAX_TOKENS]Token = undefined,

    pub fn lex(self: *Lexer, inputs: []const u8) LexError![]const Token {
        var it: Iterator(u8) = .init(inputs);
        var len: usize = 0;
        while (it.next()) |curr| : (len += 1) {
            self.tokens[len] = switch (curr) {
                '|' => Token.init(.alternation, {}),
                '*' => Token.init(.asterisk, {}),
                '\\' => Token.init(.backslash, it.next() orelse return error.UnexpectedEof),
                '^' => Token.init(.caret, {}),
                ':' => Token.init(.colon, {}),
                ',' => Token.init(.comma, {}),
                '0'...'9' => Token.init(.digit, curr),
                '$' => Token.init(.dollar, {}),
                '.' => Token.init(.dot, {}),
                '"' => Token.init(.double_quote, {}),
                '=' => Token.init(.equals, {}),
                '-' => Token.init(.hyphen, {}),
                '[' => Token.init(.left_bracket, {}),
                '{' => Token.init(.left_curly, {}),
                '(' => Token.init(.left_parenthesis, {}),
                '+' => Token.init(.plus, {}),
                '?' => Token.init(.question_mark, {}),
                ']' => Token.init(.right_bracket, {}),
                '}' => Token.init(.right_curly, {}),
                ')' => Token.init(.right_parenthesis, {}),
                '\'' => Token.init(.single_quote, {}),
                else => Token.init(.literal, curr),
            };
        }
        self.tokens[len] = Token.init(.eof, {});
        return self.tokens[0 .. len + 1];
    }

    pub const empty: Lexer = .{
        .tokens = undefined,
    };

    pub const MAX_TOKENS: usize = 256;

    pub const LexError = error{ SyntaxError, UnexpectedEof };
};

test "simple 01" {
    const regex: []const u8 = "ab";
    var lex: Lexer = .empty;

    const expecteds = [_]Token{
        .{ .literal = 'a' },
        .{ .literal = 'b' },
        .{ .eof = {} },
    };

    const tokens = try lex.lex(regex);

    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "simple 02" {
    const regex: []const u8 = "a|b";
    var lex: Lexer = .empty;

    const expecteds = [_]Token{
        .{ .literal = 'a' },
        .{ .alternation = {} },
        .{ .literal = 'b' },
        .{ .eof = {} },
    };

    const tokens = try lex.lex(regex);

    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "simple 03" {
    const regex: []const u8 = "a(b)";
    var lex: Lexer = .empty;

    const expecteds = [_]Token{
        .{ .literal = 'a' },
        .{ .left_parenthesis = {} },
        .{ .literal = 'b' },
        .{ .right_parenthesis = {} },
        .{ .eof = {} },
    };

    const tokens = try lex.lex(regex);

    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "empty input" {
    const regex: []const u8 = "";
    var lex: Lexer = .empty;

    const expecteds = [_]Token{
        .{ .eof = {} },
    };

    const tokens = try lex.lex(regex);

    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "asterisk" {
    const regex: []const u8 = "*";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .asterisk = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "backslash basic" {
    const regex: []const u8 = "\\a";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .backslash = 'a' },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "backslash escape special char" {
    const regex: []const u8 = "\\|\\*\\.";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .backslash = '|' },
        .{ .backslash = '*' },
        .{ .backslash = '.' },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "backslash at eof" {
    const regex: []const u8 = "\\";
    var lex: Lexer = .empty;

    try std.testing.expectError(Lexer.LexError.UnexpectedEof, lex.lex(regex));
}

test "caret" {
    const regex: []const u8 = "^";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .caret = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "colon" {
    const regex: []const u8 = ":";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .colon = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "comma" {
    const regex: []const u8 = ",";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .comma = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "digits" {
    const regex: []const u8 = "190";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .digit = '1' },
        .{ .digit = '9' },
        .{ .digit = '0' },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "dollar" {
    const regex: []const u8 = "$";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .dollar = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "dot" {
    const regex: []const u8 = ".";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .dot = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "double quote" {
    const regex: []const u8 = "\"";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .double_quote = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "equals" {
    const regex: []const u8 = "=";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .equals = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "hyphen" {
    const regex: []const u8 = "-";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .hyphen = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "brackets" {
    const regex: []const u8 = "[]";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .left_bracket = {} },
        .{ .right_bracket = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "curlies" {
    const regex: []const u8 = "{}";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .left_curly = {} },
        .{ .right_curly = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "plus" {
    const regex: []const u8 = "+";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .plus = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "question mark" {
    const regex: []const u8 = "?";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .question_mark = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "single quote" {
    const regex: []const u8 = "'";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .single_quote = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "mixed sequence" {
    const regex: []const u8 = "^[a-zA-Z0-9_]+\\.\\*?$";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .caret = {} },
        .{ .left_bracket = {} },
        .{ .literal = 'a' },
        .{ .hyphen = {} },
        .{ .literal = 'z' },
        .{ .literal = 'A' },
        .{ .hyphen = {} },
        .{ .literal = 'Z' },
        .{ .digit = '0' },
        .{ .hyphen = {} },
        .{ .digit = '9' },
        .{ .literal = '_' },
        .{ .right_bracket = {} },
        .{ .plus = {} },
        .{ .backslash = '.' },
        .{ .backslash = '*' },
        .{ .question_mark = {} },
        .{ .dollar = {} },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}

test "non-ascii literal" {
    const regex: []const u8 = "h\xc3\xa9llo";
    var lex: Lexer = .empty;
    const expecteds = [_]Token{
        .{ .literal = 'h' },
        .{ .literal = 0xC3 },
        .{ .literal = 0xA9 },
        .{ .literal = 'l' },
        .{ .literal = 'l' },
        .{ .literal = 'o' },
        .{ .eof = {} },
    };
    const tokens = try lex.lex(regex);
    try std.testing.expectEqual(expecteds.len, tokens.len);
    for (expecteds, tokens) |expected, token| {
        try std.testing.expectEqual(expected, token);
    }
}
