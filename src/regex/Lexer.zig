const std = @import("std");

const Token = @import("Token.zig").Token;
const Kind = Token.Kind;

pub const Lexer = struct {
    pattern: []const u8 = "",
    pos: usize = 0,

    pub fn init(pattern: []const u8) Lexer {
        return .{
            .pattern = pattern,
            .pos = 0,
        };
    }

    pub fn forward(self: *Lexer) void {
        if (self.pos < self.pattern.len) {
            self.pos += 1;
        }
    }

    pub fn backward(self: *Lexer) void {
        if (self.pos != 0) {
            self.pos -= 1;
        }
    }

    pub fn next(self: *Lexer) Token {
        const token = self.curr();

        if (self.pos < self.pattern.len) {
            self.forward();
        }
        return token;
    }

    pub fn peek(self: *Lexer) Token {
        if (self.pos >= self.pattern.len) return Token.init(.eof, {});
        // self.forward();
        const token = self.tokenize();
        // self.backward();
        return token;
    }

    pub fn curr(self: *Lexer) Token {
        return self.tokenize();
    }

    fn getch(self: *Lexer) ?u8 {
        return if (self.pos < self.pattern.len) self.pattern[self.pos] else null;
    }

    pub fn tokenize(self: *Lexer) Token {
        const c = self.getch() orelse return Token.init(.eof, {});

        return switch (c) {
            '|' => Token.init(.alternation, {}),
            '*' => Token.init(.asterisk, {}),
            '\\' => Token.init(.backslash, {}),
            ':' => Token.init(.colon, {}),
            ',' => Token.init(.comma, {}),
            '.' => Token.init(.dot, {}),
            '"' => Token.init(.double_quote, {}),
            '=' => Token.init(.equal, {}),
            '>' => Token.init(.greater_than, {}),
            '-' => Token.init(.hyphen, {}),
            '{' => Token.init(.left_brace, {}),
            '[' => Token.init(.left_bracket, {}),
            '(' => Token.init(.left_parenthesis, {}),
            '<' => Token.init(.less_than, {}),
            '+' => Token.init(.plus, {}),
            '?' => Token.init(.question_mark, {}),
            '}' => Token.init(.right_brace, {}),
            ']' => Token.init(.right_bracket, {}),
            ')' => Token.init(.right_parenthesis, {}),
            else => Token.init(.literal, c),
        };
    }
};

pub fn expectTokenSliceExact(pattern: []const u8, expected: []const Token) !void {
    var buffer_list = std.ArrayList(Token).init(std.testing.allocator);
    defer buffer_list.deinit();

    var lexer: Lexer = .init(pattern);

    while (true) {
        const token = lexer.curr();
        if (token.kind() == .eof) {
            break;
        }
        try buffer_list.append(token);
        _ = lexer.next();
    }

    try std.testing.expectEqualSlices(Token, expected, buffer_list.items);
}

test "literal" {
    try expectTokenSliceExact(
        "a",
        &.{
            .{ .literal = 'a' },
        },
    );
}

test "alternation" {
    try expectTokenSliceExact(
        "|",
        &.{
            .{ .alternation = {} },
        },
    );
}

test "asterisk" {
    try expectTokenSliceExact(
        "*",
        &.{
            .{ .asterisk = {} },
        },
    );
}

test "backslash" {
    try expectTokenSliceExact(
        "\\",
        &.{
            .{ .backslash = {} },
        },
    );
}

test "colon" {
    try expectTokenSliceExact(
        ":",
        &.{
            .{ .colon = {} },
        },
    );
}

test "comma" {
    try expectTokenSliceExact(
        ",",
        &.{
            .{ .comma = {} },
        },
    );
}

test "dot" {
    try expectTokenSliceExact(
        ".",
        &.{
            .{ .dot = {} },
        },
    );
}

test "double_quote" {
    try expectTokenSliceExact(
        "\"",
        &.{
            .{ .double_quote = {} },
        },
    );
}

test "equal" {
    try expectTokenSliceExact(
        "=",
        &.{
            .{ .equal = {} },
        },
    );
}

test "greater_than" {
    try expectTokenSliceExact(
        ">",
        &.{
            .{ .greater_than = {} },
        },
    );
}

test "hyphen" {
    try expectTokenSliceExact(
        "-",
        &.{
            .{ .hyphen = {} },
        },
    );
}

test "left_brace" {
    try expectTokenSliceExact(
        "{",
        &.{
            .{ .left_brace = {} },
        },
    );
}

test "left_bracket" {
    try expectTokenSliceExact(
        "[",
        &.{
            .{ .left_bracket = {} },
        },
    );
}

test "left_parenthesis" {
    try expectTokenSliceExact(
        "(",
        &.{
            .{ .left_parenthesis = {} },
        },
    );
}

test "less_than" {
    try expectTokenSliceExact(
        "<",
        &.{
            .{ .less_than = {} },
        },
    );
}

test "plus" {
    try expectTokenSliceExact(
        "+",
        &.{
            .{ .plus = {} },
        },
    );
}

test "question_mark" {
    try expectTokenSliceExact(
        "?",
        &.{
            .{ .question_mark = {} },
        },
    );
}

test "right_brace" {
    try expectTokenSliceExact(
        "}",
        &.{
            .{ .right_brace = {} },
        },
    );
}

test "right_bracket" {
    try expectTokenSliceExact(
        "]",
        &.{
            .{ .right_bracket = {} },
        },
    );
}

test "right_parenthesis" {
    try expectTokenSliceExact(
        ")",
        &.{
            .{ .right_parenthesis = {} },
        },
    );
}

test "empty input" {
    try expectTokenSliceExact(
        "",
        &.{},
    );
}

test "only literals" {
    try expectTokenSliceExact(
        "abc123XYZ",
        &.{
            .{ .literal = 'a' },
            .{ .literal = 'b' },
            .{ .literal = 'c' },
            .{ .literal = '1' },
            .{ .literal = '2' },
            .{ .literal = '3' },
            .{ .literal = 'X' },
            .{ .literal = 'Y' },
            .{ .literal = 'Z' },
        },
    );
}

test "only special chars" {
    try expectTokenSliceExact(
        "|*\\:,.=\"->{}[]()<+?]}",
        &.{
            .{ .alternation = {} },
            .{ .asterisk = {} },
            .{ .backslash = {} },
            .{ .colon = {} },
            .{ .comma = {} },
            .{ .dot = {} },
            .{ .equal = {} },
            .{ .double_quote = {} },
            .{ .hyphen = {} },
            .{ .greater_than = {} },
            .{ .left_brace = {} },
            .{ .right_brace = {} },
            .{ .left_bracket = {} },
            .{ .right_bracket = {} },
            .{ .left_parenthesis = {} },
            .{ .right_parenthesis = {} },
            .{ .less_than = {} },
            .{ .plus = {} },
            .{ .question_mark = {} },
            .{ .right_bracket = {} },
            .{ .right_brace = {} },
        },
    );
}

test "simple regex sequence" {
    try expectTokenSliceExact(
        "a|b*",
        &.{
            .{ .literal = 'a' },
            .{ .alternation = {} },
            .{ .literal = 'b' },
            .{ .asterisk = {} },
        },
    );
}

test "character class sequence" {
    try expectTokenSliceExact(
        "[a-z]+",
        &.{
            .{ .left_bracket = {} },
            .{ .literal = 'a' },
            .{ .hyphen = {} },
            .{ .literal = 'z' },
            .{ .right_bracket = {} },
            .{ .plus = {} },
        },
    );
}

test "mixed sequence with escapes" {
    try expectTokenSliceExact(
        "\\.\\*\\?",
        &.{
            .{ .backslash = {} },
            .{ .dot = {} },
            .{ .backslash = {} },
            .{ .asterisk = {} },
            .{ .backslash = {} },
            .{ .question_mark = {} },
        },
    );
}

test "sequence ending with literal" {
    try expectTokenSliceExact(
        "begin{.*}end",
        &.{
            .{ .literal = 'b' },
            .{ .literal = 'e' },
            .{ .literal = 'g' },
            .{ .literal = 'i' },
            .{ .literal = 'n' },
            .{ .left_brace = {} },
            .{ .dot = {} },
            .{ .asterisk = {} },
            .{ .right_brace = {} },
            .{ .literal = 'e' },
            .{ .literal = 'n' },
            .{ .literal = 'd' },
        },
    );
}

test "sequence ending with special char" {
    try expectTokenSliceExact(
        "data(value)?",
        &.{
            .{ .literal = 'd' },
            .{ .literal = 'a' },
            .{ .literal = 't' },
            .{ .literal = 'a' },
            .{ .left_parenthesis = {} },
            .{ .literal = 'v' },
            .{ .literal = 'a' },
            .{ .literal = 'l' },
            .{ .literal = 'u' },
            .{ .literal = 'e' },
            .{ .right_parenthesis = {} },
            .{ .question_mark = {} },
        },
    );
}
