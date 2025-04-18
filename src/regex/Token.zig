const std = @import("std");
const ascii = std.ascii;
const control_code = ascii.control_code;

pub const Token = u8;
pub const eof: Token = 0xFF;
pub const alt: Token = '|';
pub const lparen: Token = '(';
pub const rparen: Token = ')';
pub const lbracket: Token = '[';
pub const rbracket: Token = ']';
pub const lbrace: Token = '{';
pub const rbrace: Token = '}';
pub const quant_qmark: Token = '?';
pub const quant_plus: Token = '+';
pub const quant_star: Token = '*';
pub const dot: Token = '.';
pub const backslash: Token = '\\';

pub inline fn isUpper(self: Token) bool {
    return switch (self.item) {
        'A'...'Z' => true,
        else => false,
    };
}

pub inline fn isLower(self: Token) bool {
    return switch (self.item) {
        'a'...'z' => true,
        else => false,
    };
}

pub inline fn isAlpha(self: Token) bool {
    return switch (self.item) {
        'a'...'z' => true,
        'A'...'Z' => true,
        else => false,
    };
}

pub inline fn isDigit(self: Token) bool {
    return switch (self.item) {
        '0'...'9' => true,
        else => false,
    };
}

pub inline fn isXdigit(self: Token) bool {
    return switch (self.item) {
        '0'...'9' => true,
        'A'...'F' => true,
        'a'...'f' => true,
        else => false,
    };
}

pub inline fn isAlnum(self: Token) bool {
    return self.isAlpha() or self.isDigit();
}

pub inline fn isPunct(self: Token) bool {
    return switch (self.item) {
        33...47 => true,
        58...64 => true,
        91...96 => true,
        123...126 => true,
        else => false,
    };
}

pub inline fn isBlank(self: Token) bool {
    return switch (self.item) {
        ' ', '\t' => true,
        else => false,
    };
}

pub inline fn isSpace(self: Token) bool {
    return switch (self.item) {
        ' ', '\n', '\t', '\r', control_code.vt, control_code.ff => true,
        else => false,
    };
}

pub inline fn isCntrl(self: Token) bool {
    return switch (self.item) {
        0...31, 127 => true,
        else => false,
    };
}

pub inline fn isGraph(self: Token) bool {
    return switch (self.item) {
        33...126 => true,
        else => false,
    };
}

pub inline fn isPrint(self: Token) bool {
    return switch (self.item) {
        32...126 => true,
        else => false,
    };
}

pub inline fn isOctal(self: Token) bool {
    return switch (self.item) {
        '0'...'7' => true,
        else => false,
    };
}

pub inline fn isIdentifierStart(self: Token) bool {
    return switch (self.item) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        else => false,
    };
}

pub inline fn isIdentifierInner(self: Token) bool {
    return switch (self.item) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '0'...'9' => true,
        '_' => true,
        else => false,
    };
}

pub inline fn isEOF(self: Token) bool {
    return self == eof;
}

pub inline fn isAlternation(self: Token) bool {
    return self == '|';
}

pub inline fn isAsterisk(self: Token) bool {
    return self == '*';
}

pub inline fn isBackslash(self: Token) bool {
    return self == '\\';
}

pub inline fn isColon(self: Token) bool {
    return self == ':';
}

pub inline fn isComma(self: Token) bool {
    return self == ',';
}

pub inline fn isDot(self: Token) bool {
    return self == '.';
}

pub inline fn isDoubleQuote(self: Token) bool {
    return self == '"';
}

pub inline fn isEqualSign(self: Token) bool {
    return self == '=';
}

pub inline fn isGreaterThan(self: Token) bool {
    return self == '>';
}

pub inline fn isHyphen(self: Token) bool {
    return self == '-';
}

pub inline fn isLeftBrace(self: Token) bool {
    return self == '{';
}

pub inline fn isLeftBracket(self: Token) bool {
    return self == '[';
}

pub inline fn isLeftParenthesis(self: Token) bool {
    return self == '(';
}

pub inline fn isLessThan(self: Token) bool {
    return self == '<';
}

pub inline fn isPlus(self: Token) bool {
    return self == '+';
}

pub inline fn isQuestionMark(self: Token) bool {
    return self == '?';
}

pub inline fn isRightBrace(self: Token) bool {
    return self == '}';
}

pub inline fn isRightBracket(self: Token) bool {
    return self == ']';
}

pub inline fn isRightParenthesis(self: Token) bool {
    return self == ')';
}

pub inline fn isSlash(self: Token) bool {
    return self == '/';
}

pub inline fn isCaret(self: Token) bool {
    return self == '^';
}

pub inline fn isDollar(self: Token) bool {
    return self == '$';
}

pub inline fn isLiteral(self: Token) bool {
    @branchHint(.likely);
    return switch (self) {
        '|', '*', '\\', ':', ',', '.', '"', '=', '>', '-', '{', '[', '(', '<', '+', '?', '}', ']', ')', '^', '$', '/' => {
            @branchHint(.unlikely);
            return true;
        },
        else => {
            @branchHint(.likely);
            return true;
        },
    };
}

pub inline fn toKind(self: Token) Kind {
    return switch (self) {
        '|' => .alternation,
        '*' => .asterisk,
        '\\' => .backslash,
        ':' => .colon,
        ',' => .comma,
        '.' => .dot,
        '"' => .double_quote,
        '=' => .equal,
        '>' => .greater_than,
        '-' => .hyphen,
        '{' => .left_brace,
        '[' => .left_bracket,
        '(' => .left_parenthesis,
        '<' => .less_than,
        '+' => .plus,
        '?' => .question_mark,
        '}' => .right_brace,
        ']' => .right_bracket,
        ')' => .right_parenthesis,
        '/' => .slash,
        eof => .eof,
        else => .literal,
    };
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
};

pub fn bindingPower(self: Token) BindingPower {
    return switch (self) {
        '|' => .alternation,
        '*' => .quantifier,
        '\\' => .escaped,
        '.' => .class,
        '"' => .quoting,
        '{' => .interval_expression,
        '[' => .class,
        '(' => .grouping,
        '+' => .quantifier,
        '?' => .quantifier,
        else => if (isLiteral(self)) .concatenation else .none,
    };
}

pub const BindingPower = enum(u8) {
    none = 0,
    alternation = 1,
    interval_expression = 2,
    concatenation = 3,
    quantifier = 4,
    definition = 5,
    grouping = 6,
    quoting = 7,
    class = 8,
    escaped = 9,
};
