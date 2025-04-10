const std = @import("std");

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
};

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
