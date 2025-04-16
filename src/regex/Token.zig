const std = @import("std");

pub const Token = union(Kind) {
    alternation: void,
    asterisk: void,
    backslash: void,
    colon: void,
    comma: void,
    dot: void,
    double_quote: void,
    eof: void,
    equal: void,
    greater_than: void,
    hyphen: void,
    left_brace: void,
    left_bracket: void,
    left_parenthesis: void,
    less_than: void,
    literal: u8,
    plus: void,
    question_mark: void,
    right_brace: void,
    right_bracket: void,
    right_parenthesis: void,
    slash: void,

    pub fn init(comptime k: Kind, init_expr: anytype) Token {
        return @unionInit(Token, @tagName(k), init_expr);
    }

    pub fn kind(self: *const Token) Kind {
        return std.meta.activeTag(self.*);
    }

    pub fn toU8(self: *const Token) u8 {
        return if (self.kind().toU8()) |c| c else return self.literal;
    }

    pub fn clone(self: *const Token) Token {
        return self.*;
    }

    pub fn toLiteral(self: *Token) Token {
        self.* = Token.init(.literal, self.toU8());
        return self.*;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{c}", .{self.toU8()});
    }

    pub const Kind = enum(u8) {
        alternation,
        asterisk,
        backslash,
        colon,
        comma,
        dot,
        double_quote,
        eof,
        equal,
        greater_than,
        hyphen,
        left_brace,
        left_bracket,
        left_parenthesis,
        less_than,
        literal,
        plus,
        question_mark,
        right_brace,
        right_bracket,
        right_parenthesis,
        slash,

        pub fn toU8(self: Kind) ?u8 {
            return switch (self) {
                .alternation => '|',
                .asterisk => '*',
                .backslash => '\\',
                .colon => ':',
                .comma => ',',
                .dot => '.',
                .double_quote => '"',
                .eof => 0x00,
                .equal => '=',
                .greater_than => '>',
                .hyphen => '-',
                .left_brace => '{',
                .left_bracket => '[',
                .left_parenthesis => '(',
                .less_than => '<',
                .plus => '+',
                .question_mark => '?',
                .right_brace => '}',
                .right_bracket => ']',
                .right_parenthesis => ')',
                .slash => '/',
                else => null,
            };
        }
    };
};
