const std = @import("std");

pub const Token = union(Kind) {
    alternation: void,
    anchor_end: void,
    anchor_start: void,
    asterisk: void,
    backslash: void,
    dot: void,
    eof: void,
    left_brace: void,
    left_bracket: void,
    left_parenthesis: void,
    literal: u8,
    plus: void,
    question_mark: void,
    right_brace: void,
    right_bracket: void,
    right_parenthesis: void,
    trailing_context: void,

    pub fn init(comptime kind: Kind, init_expr: anytype) Token {
        return @unionInit(Token, @tagName(kind), init_expr);
    }

    pub fn eql(self: *const Token, token: Token) bool {
        if (self.tag() == token.tag()) {
            return switch (self.*) {
                .literal => if (self.literal == token.literal) true else false,
                else => true,
            };
        }
        return false;
    }

    // zig fmt: off
    pub const Kind = enum {
        alternation,      // '|'
        anchor_end,       // '$'
        anchor_start,     // '^'
        asterisk,         // '*'
        backslash,        // '\'
        dot,              // '.'
        eof,              // '0'
        left_brace,       // '{'
        left_bracket,     // '['
        left_parenthesis, // '('
        literal,          // 'c'
        plus,             // '+'
        question_mark,    // '?'
        right_brace,      // '}'
        right_bracket,    // ']'
        right_parenthesis,// ')'
        trailing_context, // '/'
    };
    // zig fmt: on

    pub fn tag(self: *const Token) Token.Kind {
        return std.meta.activeTag(self.*);
    }

    pub const Alternation: Token = .{ .alternation = {} };
    pub const AnchorEnd: Token = .{ .anchor_end = {} };
    pub const AnchorStart: Token = .{ .anchor_start = {} };
    pub const Asterisk: Token = .{ .asterisk = {} };
    pub const Backslash: Token = .{ .backslash = {} };
    pub const Dot: Token = .{ .dot = {} };
    pub const Eof: Token = .{ .eof = {} };
    pub const LeftBrace: Token = .{ .left_brace = {} };
    pub const LeftBracket: Token = .{ .left_bracket = {} };
    pub const LeftParenthesis: Token = .{ .left_parenthesis = {} };
    pub const Plus: Token = .{ .plus = {} };
    pub const QuestionMark: Token = .{ .question_mark = {} };
    pub const RightBrace: Token = .{ .right_brace = {} };
    pub const RightBracket: Token = .{ .right_bracket = {} };
    pub const RightParenthesis: Token = .{ .right_parenthesis = {} };
    pub const TrailingContext: Token = .{ .trailing_context = {} };
};
