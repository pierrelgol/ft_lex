const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const testing = std.testing;
const assert = std.debug.assert;
const Token = @import("Token.zig").Token;
const Lexer = @This();

pattern: []const u8,
pos: usize,
mode: Mode,

pub fn init(pattern: []const u8) Lexer {
    return .{
        .pattern = pattern,
        .pos = 0,
        .mode = .regex,
    };
}

pub fn setMode(self: *Lexer, mode: Mode) void {
    self.mode = mode;
}

pub fn getMode(self: *const Lexer) Mode {
    return self.mode;
}

pub fn isEof(self: *const Lexer) bool {
    return self.pos >= self.pattern.len;
}

pub fn advance(self: *Lexer) void {
    self.pos += if (self.isEof()) 0 else 1;
}

pub fn next(self: *Lexer) Token {
    defer self.advance();
    return self.tokenize(0);
}

pub fn curr(self: *Lexer) Token {
    return self.tokenize(0);
}

pub fn peek(self: *Lexer) Token {
    return if (self.pos + 1 < self.pattern.len) self.tokenize(1) else Token.Eof;
}

pub fn tokenize(self: *Lexer, offset: usize) Token {
    return switch (self.mode) {
        .regex => self.tokenizeRegexExpression(offset),
        .bracket => self.tokenizeBracketExpression(offset),
    };
}

fn tokenizeRegexExpression(self: *Lexer, at: usize) Token {
    if (self.isEof()) return Token.Eof;
    return switch (self.getch(at)) {
        '\\' => Token.Backslash,
        '.' => Token.Dot,
        '*' => Token.Asterisk,
        '+' => Token.Plus,
        '?' => Token.QuestionMark,
        '(' => Token.LeftParenthesis,
        ')' => Token.RightParenthesis,
        '[' => Token.LeftBracket,
        ']' => Token.RightBracket,
        '{' => Token.LeftBrace,
        '}' => Token.RightBrace,
        '^' => Token.AnchorStart,
        '$' => Token.AnchorEnd,
        '|' => Token.Alternation,
        '/' => Token.TrailingContext,
        else => Token.init(.literal, self.getch(at)),
    };
}

fn tokenizeBracketExpression(self: *Lexer, at: usize) Token {
    if (self.isEof()) return Token.Eof;
    return switch (self.getch(at)) {
        else => Token.init(.literal, self.getch(at)),
    };
}

pub fn getch(self: *const Lexer, offset: usize) u8 {
    assert(self.pos + offset < self.pattern.len);
    return self.pattern[self.pos + offset];
}

pub const Mode = enum { regex, bracket };

pub fn expectToken(expected: []const Token.Kind, lexer: *Lexer) !void {
    var array = std.BoundedArray(Token.Kind, 256).init(0) catch unreachable;
    while (lexer.curr().tag() != .eof) : (lexer.advance()) {
        array.appendAssumeCapacity(lexer.curr().tag());
    }
    try testing.expectEqualSlices(Token.Kind, expected, array.constSlice());
}

test "literals" {
    const input: []const u8 = "ab";
    const expected = [_]Token.Kind{
        .literal,
        .literal,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "literal" {
    const input: []const u8 = "a";
    const expected = [_]Token.Kind{
        .literal,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "alternation" {
    const input: []const u8 = "|";
    const expected = [_]Token.Kind{
        .alternation,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "anchor_end" {
    const input: []const u8 = "$";
    const expected = [_]Token.Kind{
        .anchor_end,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "anchor_start" {
    const input: []const u8 = "^";
    const expected = [_]Token.Kind{
        .anchor_start,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "asterisk" {
    const input: []const u8 = "*";
    const expected = [_]Token.Kind{
        .asterisk,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "backslash" {
    const input: []const u8 = "\\";
    const expected = [_]Token.Kind{
        .backslash,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "dot" {
    const input: []const u8 = ".";
    const expected = [_]Token.Kind{
        .dot,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "eof" {
    const input: []const u8 = "";
    const expected = [_]Token.Kind{};
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "left_brace" {
    const input: []const u8 = "{";
    const expected = [_]Token.Kind{
        .left_brace,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "left_bracket" {
    const input: []const u8 = "[";
    const expected = [_]Token.Kind{
        .left_bracket,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "left_parenthesis" {
    const input: []const u8 = "(";
    const expected = [_]Token.Kind{
        .left_parenthesis,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "plus" {
    const input: []const u8 = "+";
    const expected = [_]Token.Kind{
        .plus,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "question_mark" {
    const input: []const u8 = "?";
    const expected = [_]Token.Kind{
        .question_mark,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "right_brace" {
    const input: []const u8 = "}";
    const expected = [_]Token.Kind{
        .right_brace,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "right_bracket" {
    const input: []const u8 = "]";
    const expected = [_]Token.Kind{
        .right_bracket,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "right_parenthesis" {
    const input: []const u8 = ")";
    const expected = [_]Token.Kind{
        .right_parenthesis,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}

test "trailing_context" {
    const input: []const u8 = "/";
    const expected = [_]Token.Kind{
        .trailing_context,
    };
    var lexer: Lexer = .init(input);
    try expectToken(&expected, &lexer);
}
