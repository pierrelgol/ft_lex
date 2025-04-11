const std = @import("std");
const common = @import("common.zig");
const Iterator = common.Iterator;
const Lexer = @This();

pub const Error = error{ SyntaxError, UnexpectedEof };
pub const Context = struct {
    tokenize: *const fn (tokenizer: *Lexer) Error!Token = undefined,

    pub const regex: Context = .{ .tokenize = tokenizeRegexExpression };
    pub const class: Context = .{ .tokenize = tokenizeClassExpression };
};

ctx: Context = Context.regex,
it: Iterator(u8) = undefined,

pub fn init(ctx: Context, input: []const u8) Lexer {
    return .{
        .ctx = ctx,
        .it = Iterator(u8).init(input),
    };
}

pub fn setContext(self: *Lexer, ctx: enum { regex, class }) void {
    self.ctx = if (ctx == .regex) Context.regex else Context.class;
}

pub fn next(self: *Lexer) Error!Token {
    return try self.ctx.tokenize(self);
}

pub fn peek(self: *Lexer) Error!Token {
    self.it.save();
    defer self.it.restore();
    const token = try self.ctx.tokenize(self);
    return token;
}

fn tokenizeRegexExpression(self: *Lexer) Error!Token {
    const next_token = self.it.next() orelse return Token.Eof;
    return switch (next_token) {
        '\\' => Token.init(.backslash, self.it.next() orelse return error.UnexpectedEof),
        '.' => Token.Dot,
        '*' => Token.Asterisk,
        '+' => Token.Plus,
        '?' => Token.QuestionMark,
        '{' => Token.LeftCurly,
        '[' => Token.LeftBracket,
        '(' => Token.LeftParenthesis,
        '}' => Token.RightCurly,
        ']' => Token.RightBracket,
        ')' => Token.RightParenthesis,
        '^' => Token.Caret,
        ',' => Token.Comma,
        ':' => Token.Colon,
        '$' => Token.Dollar,
        '|' => Token.Alternation,
        '=' => Token.Equals,
        '\'' => Token.SingleQuote,
        '"' => Token.DoubleQuote,
        '0'...'9' => Token.init(.digit, next_token),
        else => Token.init(.literal, next_token),
    };
}

fn tokenizeClassExpression(self: *Lexer) Error!Token {
    const next_token = self.it.next() orelse return Token.Eof;
    return switch (next_token) {
        '\\' => Token.init(.backslash, self.it.next() orelse return error.UnexpectedEof),
        else => Token.init(.literal, next_token),
    };
}

pub const Token = union(Kind) {
    alternation: void,
    asterisk: void,
    backslash: u8,
    caret: void,
    colon: void,
    comma: void,
    digit: u8,
    dollar: void,
    dot: void,
    double_quote: void,
    eof: void,
    equals: void,
    hyphen: void,
    left_bracket: void,
    left_curly: void,
    left_parenthesis: void,
    literal: u8,
    plus: void,
    question_mark: void,
    right_bracket: void,
    right_curly: void,
    right_parenthesis: void,
    single_quote: void,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        const elem: []const u8 = switch (self) {
            .alternation => "|",
            .asterisk => "*",
            .backslash => &[_]u8{self.backslash},
            .caret => "^",
            .colon => ":",
            .comma => ",",
            .digit => &[_]u8{self.digit},
            .dollar => "$",
            .dot => ".",
            .double_quote => "\"",
            .eof => "EOF",
            .equals => "=",
            .hyphen => "-",
            .left_bracket => "[",
            .left_curly => "{",
            .left_parenthesis => "(",
            .literal => &[_]u8{self.literal},
            .plus => "+",
            .question_mark => "?",
            .right_bracket => "]",
            .right_curly => "}",
            .right_parenthesis => ")",
            .single_quote => "\'",
        };

        try writer.print("'{s}'", .{elem});
    }

    pub fn init(comptime kind: Kind, value: anytype) Token {
        return @unionInit(Token, @tagName(kind), value);
    }

    pub const Alternation: Token = .init(.alternation, {});
    pub const Eof: Token = .init(.eof, {});
    pub const Asterisk: Token = .init(.asterisk, {});
    pub const Caret: Token = .init(.caret, {});
    pub const Colon: Token = .init(.colon, {});
    pub const Comma: Token = .init(.comma, {});
    pub const Dollar: Token = .init(.dollar, {});
    pub const Dot: Token = .init(.dot, {});
    pub const DoubleQuote: Token = .init(.double_quote, {});
    pub const Equals: Token = .init(.equals, {});
    pub const Hyphen: Token = .init(.hyphen, {});
    pub const LeftBracket: Token = .init(.left_bracket, {});
    pub const LeftCurly: Token = .init(.left_curly, {});
    pub const LeftParenthesis: Token = .init(.left_parenthesis, {});
    pub const Plus: Token = .init(.plus, {});
    pub const QuestionMark: Token = .init(.question_mark, {});
    pub const RightBracket: Token = .init(.right_bracket, {});
    pub const RightCurly: Token = .init(.right_curly, {});
    pub const RightParenthesis: Token = .init(.right_parenthesis, {});
    pub const SingleQuote: Token = .init(.single_quote, {});

    pub const Kind = enum {
        alternation,
        asterisk,
        backslash,
        caret,
        colon,
        comma,
        digit,
        dollar,
        dot,
        double_quote,
        eof,
        equals,
        hyphen,
        left_bracket,
        left_curly,
        left_parenthesis,
        literal,
        plus,
        question_mark,
        right_bracket,
        right_curly,
        right_parenthesis,
        single_quote,
    };
};

test "simple 01" {
    const regex: []const u8 = "ab";
    var lex = Lexer.init(Context.regex, regex);

    const expecteds = [_]Token{
        Token.init(.literal, 'a'),
        Token.init(.literal, 'b'),
        Token.Eof,
    };

    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "simple 02" {
    const regex: []const u8 = "a|b";
    var lex = Lexer.init(Context.regex, regex);

    const expecteds = [_]Token{
        Token.init(.literal, 'a'),
        Token.Alternation,
        Token.init(.literal, 'b'),
        Token.Eof,
    };

    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "simple 03" {
    const regex: []const u8 = "a(b)";
    var lex = Lexer.init(Context.regex, regex);

    const expecteds = [_]Token{
        Token.init(.literal, 'a'),
        Token.LeftParenthesis,
        Token.init(.literal, 'b'),
        Token.RightParenthesis,
        Token.Eof,
    };

    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "empty input" {
    const regex: []const u8 = "";
    var lex = Lexer.init(Context.regex, regex);

    const expecteds = [_]Token{
        Token.Eof,
    };

    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }

    try std.testing.expectEqual(Token.Eof, try lex.next());
}

test "asterisk" {
    const regex: []const u8 = "*";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Asterisk,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "backslash basic" {
    const regex: []const u8 = "\\a";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.backslash, 'a'),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "backslash escape special char" {
    const regex: []const u8 = "\\|\\*\\.";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.backslash, '|'),
        Token.init(.backslash, '*'),
        Token.init(.backslash, '.'),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "backslash at eof" {
    const regex: []const u8 = "\\";
    var lex = Lexer.init(Context.regex, regex);

    try std.testing.expectError(Lexer.Error.UnexpectedEof, lex.next());
}

test "caret" {
    const regex: []const u8 = "^";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Caret,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "colon" {
    const regex: []const u8 = ":";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Colon,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "comma" {
    const regex: []const u8 = ",";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Comma,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "digits" {
    const regex: []const u8 = "190";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.digit, '1'),
        Token.init(.digit, '9'),
        Token.init(.digit, '0'),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "dollar" {
    const regex: []const u8 = "$";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Dollar,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "dot" {
    const regex: []const u8 = ".";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Dot,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "double quote as literal" {
    const regex: []const u8 = "\"";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.double_quote, {}),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "equals" {
    const regex: []const u8 = "=";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Equals,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "hyphen as literal" {
    const regex: []const u8 = "-";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.literal, '-'),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "brackets" {
    const regex: []const u8 = "[]";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.LeftBracket,
        Token.RightBracket,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "curlies" {
    const regex: []const u8 = "{}";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.LeftCurly,
        Token.RightCurly,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "plus" {
    const regex: []const u8 = "+";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Plus,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "question mark" {
    const regex: []const u8 = "?";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.QuestionMark,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "single quote as literal" {
    const regex: []const u8 = "'";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.single_quote, {}),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "mixed sequence" {
    const regex: []const u8 = "^[a-zA-Z0-9_]+\\.\\*?$";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.Caret,
        Token.LeftBracket,
        Token.init(.literal, 'a'),
        Token.init(.literal, '-'),
        Token.init(.literal, 'z'),
        Token.init(.literal, 'A'),
        Token.init(.literal, '-'),
        Token.init(.literal, 'Z'),
        Token.init(.digit, '0'),
        Token.init(.literal, '-'),
        Token.init(.digit, '9'),
        Token.init(.literal, '_'),
        Token.RightBracket,
        Token.Plus,
        Token.init(.backslash, '.'),
        Token.init(.backslash, '*'),
        Token.QuestionMark,
        Token.Dollar,
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();

        try std.testing.expectEqual(expected, actual);
    }
}

test "non-ascii literal" {
    const regex: []const u8 = "h\xc3\xa9llo";
    var lex = Lexer.init(Context.regex, regex);
    const expecteds = [_]Token{
        Token.init(.literal, 'h'),
        Token.init(.literal, 0xC3),
        Token.init(.literal, 0xA9),
        Token.init(.literal, 'l'),
        Token.init(.literal, 'l'),
        Token.init(.literal, 'o'),
        Token.Eof,
    };
    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "class context basic" {
    const input: []const u8 = "a-z\\d";
    var lex = Lexer.init(Context.class, input);

    const expecteds = [_]Token{
        Token.init(.literal, 'a'),
        Token.init(.literal, '-'),
        Token.init(.literal, 'z'),
        Token.init(.backslash, 'd'),
        Token.Eof,
    };

    for (expecteds) |expected| {
        const actual = try lex.next();
        try std.testing.expectEqual(expected, actual);
    }
}

test "class context backslash eof" {
    const input: []const u8 = "\\";
    var lex = Lexer.init(Context.class, input);
    try std.testing.expectError(Lexer.Error.UnexpectedEof, lex.next());
}

test "peek does not advance" {
    const regex: []const u8 = "ab";
    var lex = Lexer.init(Context.regex, regex);

    const peek1 = try lex.peek();
    try std.testing.expectEqual(Token.init(.literal, 'a'), peek1);

    const peek2 = try lex.peek();
    try std.testing.expectEqual(Token.init(.literal, 'a'), peek2);

    const next1 = try lex.next();
    try std.testing.expectEqual(Token.init(.literal, 'a'), next1);

    const peek3 = try lex.peek();
    try std.testing.expectEqual(Token.init(.literal, 'b'), peek3);

    const next2 = try lex.next();
    try std.testing.expectEqual(Token.init(.literal, 'b'), next2);

    const peek4 = try lex.peek();
    try std.testing.expectEqual(Token.Eof, peek4);

    const next3 = try lex.next();
    try std.testing.expectEqual(Token.Eof, next3);

    const peek5 = try lex.peek();
    try std.testing.expectEqual(Token.Eof, peek5);
}
