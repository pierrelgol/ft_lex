const std = @import("std");
const mem = std.mem;
const math = std.math;
const Order = math.Order;
const assert = std.debug.assert;
const Allocator = mem.Allocator;
const Range = std.bit_set.Range;
const ascii = std.ascii;
const control_code = ascii.control_code;
const testing = std.testing;

const Ast = @import("Ast.zig").Ast;
const Node = Ast.Node;
const Buffer256 = std.BoundedArray(u8, 256);
const Charset = std.bit_set.IntegerBitSet(256);
const Token = u8;
const Eof: Token = 0xFF;
const DoubleQuote: Token = '"';
const Alternation: Token = '|';
const LeftParenthesis: Token = '(';
const RightParenthesis: Token = ')';
const LeftBracket: Token = '[';
const RightBracket: Token = ']';
const LeftBrace: Token = '{';
const RightBrace: Token = '}';
const QuestionMark: Token = '?';
const Plus: Token = '+';
const Asterisk: Token = '*';
const Dot: Token = '.';
const Colon: Token = ':';
const Comma: Token = ',';
const Equals: Token = '=';
const GreaterThan: Token = '>';
const LessThan: Token = '<';
const Hyphen: Token = '-';
const Backslash: Token = '\\';

const Parser = @This();

buf: []const u8,
pos: usize,
tok: Token,
len: usize,
ast: Ast,

pub const Error = error{
    SyntaxError,
    UnexpectedToken,
    UnexpectedEof,
    EmptyGroup,
    EmptyClass,
    EmptyQuote,
    EmptyDefinition,
    EmptyInterval,
    UnclosedBracket,
    UnclosedBrace,
    UnclosedQuote,
    UnclosedParenthesis,
} || Allocator.Error;

pub fn init(gpa: Allocator, pattern: []const u8) Parser {
    const token = if (pattern.len == 0) Eof else pattern[0];
    return .{
        .buf = pattern,
        .pos = 0,
        .ast = Ast.init(gpa),
        .len = 0,
        .tok = token,
    };
}

pub fn parse(self: *Parser) Error!Ast {
    if (isEOF(self.tok)) {
        self.ast.root = null;
    } else {
        self.ast.root = try self.parseExpression(.none);
    }
    return self.ast;
}

pub fn deinit(self: *Parser) void {
    self.ast.deinit();
}

inline fn forward(self: *Parser) void {
    if (self.pos < self.buf.len) {
        self.pos += 1;
        self.len += 1;
    }
}

inline fn backward(self: *Parser) void {
    if (self.pos != 0) {
        self.pos -= 1;
        self.len -= 1;
    }
}

fn eats(self: *Parser, n: usize) void {
    while (n != 0) _ = self.next();
}

inline fn next(self: *Parser) Token {
    self.forward();
    const token = self.tok;
    self.tok = self.current();
    return token;
}

inline fn peek(self: *const Parser) Token {
    return if (self.pos + 1 < self.buf.len) self.buf[self.pos + 1] else Eof;
}

inline fn current(self: *const Parser) Token {
    return if (self.pos < self.buf.len) self.buf[self.pos] else Eof;
}

inline fn orderBindingPower(cur_bp: BindingPower, min_bp: BindingPower) Order {
    return math.order(cur_bp.toU8(), min_bp.toU8());
}

fn createNode(self: *Parser, init_expr: Node) Allocator.Error!*Node {
    const node = try self.ast.createNode();
    node.* = init_expr;
    return node;
}

fn parseExpression(self: *Parser, min_bp: BindingPower) Error!*Node {
    var left: *Node = try self.nud();
    var cur_bp: BindingPower = toBindingPower(self.tok);

    while (!isEOF(self.tok) and orderBindingPower(cur_bp, min_bp) == .gt) {
        left = try self.led(left);
        cur_bp = toBindingPower(self.tok);
    }

    return left;
}

fn nud(self: *Parser) Error!*Node {
    return switch (self.tok) {
        Dot => self.nudDot(),
        LeftBracket => self.nudClass(),
        LeftParenthesis => self.nudGroup(),
        DoubleQuote => self.nudQuoted(),
        Backslash => self.nudEscaped(),
        LeftBrace => error.SyntaxError,
        Alternation => error.SyntaxError,
        Asterisk => error.SyntaxError,
        Colon => error.SyntaxError,
        Comma => error.SyntaxError,
        Equals => error.SyntaxError,
        GreaterThan => error.SyntaxError,
        Hyphen => error.SyntaxError,
        LessThan => error.SyntaxError,
        Plus => error.SyntaxError,
        QuestionMark => error.SyntaxError,
        RightBrace => error.SyntaxError,
        RightBracket => error.SyntaxError,
        RightParenthesis => error.SyntaxError,
        Eof => error.UnexpectedEof,
        else => self.nudLiteral(),
    };
}

fn nudLiteral(self: *Parser) Error!*Node {
    assert(isLiteral(self.tok));
    return self.createNode(.{
        .literal = self.next(),
    });
}

fn nudGroup(self: *Parser) Error!*Node {
    assert(isLeftParenthesis(self.tok));
    self.eats(1);

    if (isRightParenthesis(self.tok))
        return error.EmptyGroup;

    const group = try self.parseExpression(.none);

    if (isEOF(self.tok))
        return error.UnclosedParenthesis
    else
        self.eats(1);
    return self.createNode(.{
        .group = group,
    });
}

fn nudQuoted(self: *Parser) Error!*Node {
    assert(isDoubleQuote(self.tok));
    self.eats(1);
    var buffer = Buffer256.init(0) catch unreachable;
    while (true) : (buffer.appendAssumeCapacity(self.next())) {
        if (isEOF(self.tok)) return error.UnexpectedEof;
        if (isDoubleQuote(self.tok)) break;
    }
    assert(isDoubleQuote(self.tok));
    self.eats(1);
    const allocator = self.ast.scratchAllocator();
    const string = try allocator.dupe(u8, buffer.constSlice());
    return self.createNode(.{
        .quoted = .{
            .string = string,
        },
    });
}

fn nudDot(self: *Parser) Error!*Node {
    assert(isDot(self.tok));
    return self.createNode(.{
        .class = .{
            .negated = false,
            .charset = charset: {
                var value: Charset = .initFull();
                value.setValue('\n', false);
                break :charset value;
            },
        },
    });
}

fn nudClass(self: *Parser) Error!*Node {
    assert(isLeftBracket(self.tok));
    self.eats(1);
    var negated: bool = false;
    var charset: Charset = .initEmpty();
    if (isCaret(self.tok)) {
        negated = true;
        self.eats(1);
    }
    if (negated and isRightBracket(self.tok) and isEOF(self.peek())) {
        self.eats(1);
        charset.set('^');
        return self.createNode(.{
            .class = .{
                .negated = false,
                .charset = charset,
            },
        });
    }
    if (isRightBracket(self.tok) or isHyphen(self.tok) and !isEOF(self.peek())) {
        charset.set(self.next());
    }
    if (isEOF(self.tok)) {
        return error.EmptyClass;
    }
    while (true) {
        if (isEOF(self.tok)) return error.UnclosedBracket;
        if (isRightBracket(self.tok)) break;
        if (isLeftBracket(self.tok) and isColon(self.peek())) {
            const class = try self.parsePosixClass();
            charset.setUnion(class);
            continue;
        }
        if (isBackslash(self.tok)) {
            const escaped = try self.parseEscaped();
            charset.set(escaped);
            continue;
        }
        if (isHyphen(self.peek())) {
            var range: Range = .{
                .start = self.tok,
                .end = 0,
            };
            self.eats(2);
            if (isRightBracket(self.tok)) {
                charset.set(range.start);
                charset.set('-');
                continue;
            }
            range.end = self.tok;
            if (range.end < range.start) {
                charset.set(range.start);
                charset.set('-');
                charset.set(range.end);
            } else {
                charset.setRangeValue(range, true);
            }
            continue;
        }
    }
    assert(isRightBracket(self.tok));
    self.eats(1);
    return self.createNode(.{
        .class = .{
            .negated = negated,
            .charset = charset,
        },
    });
}

fn nudEscaped(self: *Parser) Error!*Node {
    assert(isBackslash(self.tok));
    const escaped = try self.parseEscaped();
    return self.createNode(.{
        .literal = escaped,
    });
}

fn led(self: *Parser, left: *Node) Error!*Node {
    return switch (self.tok) {
        '+' => self.ledOneOrMore(left),
        '?' => self.ledZeroOrOne(left),
        '*' => self.ledZeroOrMore(left),
        '(' => self.ledImplicitConcat(left),
        '.' => self.ledImplicitConcat(left),
        '[' => self.ledImplicitConcat(left),
        '"' => self.ledImplicitConcat(left),
        '\\' => self.ledImplicitConcat(left),
        '|' => self.ledAlternation(left),
        ':' => error.SyntaxError,
        ',' => error.SyntaxError,
        '=' => error.SyntaxError,
        '>' => error.SyntaxError,
        '-' => error.SyntaxError,
        '<' => error.SyntaxError,
        '}' => error.SyntaxError,
        ']' => error.SyntaxError,
        ')' => error.SyntaxError,
        '{' => self.ledIntervalExpression(left),
        Eof => error.UnexpectedEof,
        else => self.ledImplicitConcat(left),
    };
}

fn ledImplicitConcat(self: *Parser, left: *Node) Error!*Node {
    return self.createNode(.{
        .concat = .{
            .lhs = left,
            .rhs = try self.parseExpression(.concatenation),
        },
    });
}

fn ledZeroOrOne(self: *Parser, left: *Node) Error!*Node {
    assert(isQuestionMark(self.tok));
    self.eats(1);
    return self.createNode(.{
        .quantifier = .{
            .min = 0,
            .max = 1,
            .lhs = left,
        },
    });
}

fn ledOneOrMore(self: *Parser, left: *Node) Error!*Node {
    assert(isPlus(self.tok));
    self.eats(1);
    return self.createNode(.{
        .quantifier = .{
            .min = 1,
            .max = null,
            .lhs = left,
        },
    });
}

fn ledZeroOrMore(self: *Parser, left: *Node) Error!*Node {
    assert(isAsterisk(self.tok));
    self.eats(1);
    return self.createNode(.{
        .quantifier = .{
            .min = 0,
            .max = null,
            .lhs = left,
        },
    });
}

fn ledIntervalExpression(self: *Parser, left: *Node) Error!*Node {
    assert(isLeftBrace(self.tok));
    self.eats(1);

    const min = try self.parseInterval();

    if (isRightBrace(self.tok)) {
        self.eats(1);
        return self.createNode(.{
            .quantifier = .{
                .min = min,
                .max = min,
                .lhs = left,
            },
        });
    }

    if (isComma(self.tok))
        self.eats(1)
    else
        return error.SyntaxError;

    if (isRightBrace(self.tok)) {
        return self.createNode(.{
            .quantifier = .{
                .min = min,
                .max = null,
                .lhs = left,
            },
        });
    }

    const max = try self.parseInterval();

    if (isRightBrace(self.tok))
        self.eats(1)
    else
        return error.UnclosedBrace;

    return self.createNode(.{ .quantifier = .{
        .min = min,
        .max = max,
        .lhs = left,
    } });
}

fn ledAlternation(self: *Parser, left: *Node) Error!*Node {
    assert(isAlternation(self.tok));
    self.eats(1);
    return self.createNode(.{
        .alternation = .{
            .lhs = left,
            .rhs = try self.parseExpression(.alternation),
        },
    });
}

fn parseInterval(self: *Parser) Error!usize {
    if (!isDigit(self.tok)) return error.SyntaxError;

    var buffer = Buffer256.init(0) catch unreachable;

    while (true) : (buffer.appendAssumeCapacity(self.next())) {
        if (isEOF(self.tok)) return error.UnclosedBrace;
        if (isComma(self.tok) or isRightBrace(self.tok)) break;
        if (!isDigit(self.tok)) return error.SyntaxError;
    }
    return std.fmt.parseUnsigned(usize, buffer.constSlice(), 10) catch {
        return error.SyntaxError;
    };
}

fn parseHexadecimal(self: *Parser) Error!Token {
    assert(self.tok == 'x' or self.tok == 'X');
    self.eats(1);

    var buffer = Buffer256.init(0) catch unreachable;
    for (0..2) |_| {
        if (isHex(self.tok))
            buffer.appendAssumeCapacity(self.next())
        else
            break;
    }

    return std.fmt.parseUnsigned(u8, buffer.constSlice(), 16) catch {
        return error.SyntaxError;
    };
}

fn parseOctal(self: *Parser, len: usize) Error!Token {
    assert(isOctal(self.tok) or self.tok == 'o');
    if (len == 2) self.eats(1);

    var buffer = Buffer256.init(0) catch unreachable;
    for (0..len) |_| {
        if (isOctal(self.tok))
            buffer.appendAssumeCapacity(self.next())
        else
            break;
    }

    return std.fmt.parseUnsigned(u8, buffer.constSlice(), 8) catch {
        return error.SyntaxError;
    };
}

fn parseEscaped(self: *Parser) Error!Token {
    assert(isBackslash(self.tok));
    self.eats(1);
    const token = self.tok;
    return switch (token) {
        Eof => tok: {
            break :tok error.UnexpectedEof;
        },
        'n' => tok: {
            self.eats(1);
            break :tok '\n';
        },
        't' => tok: {
            self.eats(1);
            break :tok '\t';
        },
        'r' => tok: {
            self.eats(1);
            break :tok '\r';
        },
        'f' => tok: {
            self.eats(1);
            break :tok control_code.ff;
        },
        'v' => tok: {
            self.eats(1);
            break :tok control_code.vt;
        },
        '0'...'7' => tok: {
            break :tok try self.parseOctal(3);
        },
        'o' => tok: {
            break :tok try self.parseOctal(2);
        },
        'x', 'X' => tok: {
            break :tok try self.parseHexadecimal();
        },
        else => tok: {
            self.eats(1);
            break :tok token;
        },
    };
}

fn parsePosixClass(self: *Parser) Error!Charset {
    assert(isLeftBracket(self.tok) and isColon(self.peek()));
    self.eats(2);

    var buffer = Buffer256.init(0) catch unreachable;
    while (true) : (buffer.appendAssumeCapacity(self.next())) {
        if (isColon(self.tok) and isRightBracket(self.peek())) {
            self.eats(2);
            break;
        }
    }
    const class = std.meta.stringToEnum(PosixClass, buffer.constSlice()) orelse return error.SyntaxError;
    return class.toCharset();
}

pub inline fn isUpper(self: Token) bool {
    return switch (self) {
        'A'...'Z' => true,
        else => false,
    };
}

pub inline fn isLower(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        else => false,
    };
}

pub inline fn isAlpha(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        else => false,
    };
}

pub inline fn isDigit(self: Token) bool {
    return switch (self) {
        '0'...'9' => true,
        else => false,
    };
}

pub inline fn isHex(self: Token) bool {
    return switch (self) {
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
    return switch (self) {
        33...47 => true,
        58...64 => true,
        91...96 => true,
        123...126 => true,
        else => false,
    };
}

pub inline fn isBlank(self: Token) bool {
    return switch (self) {
        ' ', '\t' => true,
        else => false,
    };
}

pub inline fn isSpace(self: Token) bool {
    return switch (self) {
        ' ', '\n', '\t', '\r', control_code.vt, control_code.ff => true,
        else => false,
    };
}

pub inline fn isCntrl(self: Token) bool {
    return switch (self) {
        0...31, 127 => true,
        else => false,
    };
}

pub inline fn isGraph(self: Token) bool {
    return switch (self) {
        33...126 => true,
        else => false,
    };
}

pub inline fn isPrint(self: Token) bool {
    return switch (self) {
        32...126 => true,
        else => false,
    };
}

pub inline fn isOctal(self: Token) bool {
    return switch (self) {
        '0'...'7' => true,
        else => false,
    };
}

pub inline fn isIdentifierStart(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        else => false,
    };
}

pub inline fn isIdentifierInner(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '0'...'9' => true,
        '_' => true,
        else => false,
    };
}

pub inline fn isEOF(self: Token) bool {
    return self == Eof;
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

pub fn toBindingPower(self: Token) BindingPower {
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

    pub fn toU8(self: BindingPower) u8 {
        return @intFromEnum(self);
    }
};

pub const PosixClass = enum {
    upper,
    lower,
    alpha,
    digit,
    xdigit,
    alnum,
    punct,
    blank,
    space,
    cntrl,
    graph,
    print,

    pub fn toCharset(class: PosixClass) Charset {
        const set = switch (class) {
            .upper => upper_chars,
            .lower => lower_chars,
            .alpha => alpha_chars,
            .digit => digit_chars,
            .xdigit => xdigit_chars,
            .alnum => alnum_chars,
            .punct => punct_chars,
            .blank => blank_chars,
            .space => space_chars,
            .cntrl => cntrl_chars,
            .graph => graph_chars,
            .print => print_chars,
        };

        var charset: Charset = .initEmpty();
        for (set) |value| {
            charset.set(value);
        }

        return charset;
    }
};

const upper_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const lower_chars = "abcdefghijklmnopqrstuvwxyz";
const alpha_chars = upper_chars ++ lower_chars;
const digit_chars = "0123456789";
const xdigit_chars = digit_chars ++ "abcdef" ++ "ABCDEF";
const alnum_chars = alpha_chars ++ digit_chars;
const punct_chars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
const blank_chars = " \t";
const space_chars = std.ascii.whitespace ++ "";
const graph_chars = alnum_chars ++ punct_chars;
const print_chars = graph_chars ++ " ";
const cntrl_chars = blk: {
    var buffer = std.BoundedArray(u8, 33).init(0) catch unreachable;
    for (0..31) |i| {
        buffer.appendAssumeCapacity(@intCast(i));
    }
    buffer.appendAssumeCapacity(std.ascii.control_code.del);
    break :blk buffer.constSlice().ptr[0..33] ++ "";
};

test Parser {
    const gpa = testing.allocator;
    const pattern: []const u8 = "";
    var parser: Parser = .init(gpa, pattern);
    defer parser.deinit();

    _ = try parser.parse();
}
