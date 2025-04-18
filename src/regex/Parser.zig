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
    }
}

fn eats(self: *Parser, n: usize) void {
    for (0..n) |_| _ = self.next();
}

inline fn next(self: *Parser) Token {
    if (self.pos >= self.buf.len) return Eof;
    self.pos += 1;
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
        Alternation => error.SyntaxError,
        Asterisk => error.SyntaxError,
        Plus => error.SyntaxError,
        QuestionMark => error.SyntaxError,
        LeftBrace => error.SyntaxError,
        Eof => error.UnexpectedEof,
        else => self.nudLiteral(),
    };
}

fn nudLiteral(self: *Parser) Error!*Node {
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
    self.eats(1);
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

    if (negated and isRightBracket(self.tok) and (isLeftBracket(self.peek()) or isEOF(self.peek()))) {
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
        if (isRightBracket(self.tok)) {
            break;
        } else if (isLeftBracket(self.tok) and isColon(self.peek())) {
            const class = try self.parsePosixClass();
            charset.setUnion(class);
            continue;
        } else if (isBackslash(self.tok)) {
            const escaped = try self.parseEscaped();
            charset.set(escaped);
            continue;
        } else if (isHyphen(self.peek())) {
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
        charset.set(self.next());
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
        Plus => self.ledOneOrMore(left),
        QuestionMark => self.ledZeroOrOne(left),
        Asterisk => self.ledZeroOrMore(left),
        LeftParenthesis => self.ledImplicitConcat(left),
        Dot => self.ledImplicitConcat(left),
        LeftBracket => self.ledImplicitConcat(left),
        DoubleQuote => self.ledImplicitConcat(left),
        Backslash => self.ledImplicitConcat(left),
        Alternation => self.ledAlternation(left),
        LeftBrace => self.ledIntervalExpression(left),
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
        self.eats(1);
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

    if (isEOF(self.tok)) return error.UnexpectedEof;
    if (self.tok == Alternation) return error.SyntaxError;
    if (isPlus(self.tok) or isAsterisk(self.tok) or isQuestionMark(self.tok) or isLeftBrace(self.tok)) {
        return error.SyntaxError;
    }

    const rhs = try self.parseExpression(.alternation);
    return self.createNode(.{ .alternation = .{
        .lhs = left,
        .rhs = rhs,
    } });
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

inline fn isUpper(self: Token) bool {
    return switch (self) {
        'A'...'Z' => true,
        else => false,
    };
}

inline fn isLower(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        else => false,
    };
}

inline fn isAlpha(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        else => false,
    };
}

inline fn isDigit(self: Token) bool {
    return switch (self) {
        '0'...'9' => true,
        else => false,
    };
}

inline fn isHex(self: Token) bool {
    return switch (self) {
        '0'...'9' => true,
        'A'...'F' => true,
        'a'...'f' => true,
        else => false,
    };
}

inline fn isAlnum(self: Token) bool {
    return self.isAlpha() or self.isDigit();
}

inline fn isPunct(self: Token) bool {
    return switch (self) {
        33...47 => true,
        58...64 => true,
        91...96 => true,
        123...126 => true,
        else => false,
    };
}

inline fn isBlank(self: Token) bool {
    return switch (self) {
        ' ', '\t' => true,
        else => false,
    };
}

inline fn isSpace(self: Token) bool {
    return switch (self) {
        ' ', '\n', '\t', '\r', control_code.vt, control_code.ff => true,
        else => false,
    };
}

inline fn isCntrl(self: Token) bool {
    return switch (self) {
        0...31, 127 => true,
        else => false,
    };
}

inline fn isGraph(self: Token) bool {
    return switch (self) {
        33...126 => true,
        else => false,
    };
}

inline fn isPrint(self: Token) bool {
    return switch (self) {
        32...126 => true,
        else => false,
    };
}

inline fn isOctal(self: Token) bool {
    return switch (self) {
        '0'...'7' => true,
        else => false,
    };
}

inline fn isIdentifierStart(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        else => false,
    };
}

inline fn isIdentifierInner(self: Token) bool {
    return switch (self) {
        'a'...'z' => true,
        'A'...'Z' => true,
        '0'...'9' => true,
        '_' => true,
        else => false,
    };
}

inline fn isEOF(self: Token) bool {
    return self == Eof;
}

inline fn isAlternation(self: Token) bool {
    return self == '|';
}

inline fn isAsterisk(self: Token) bool {
    return self == '*';
}

inline fn isBackslash(self: Token) bool {
    return self == '\\';
}

inline fn isColon(self: Token) bool {
    return self == ':';
}

inline fn isComma(self: Token) bool {
    return self == ',';
}

inline fn isDot(self: Token) bool {
    return self == '.';
}

inline fn isDoubleQuote(self: Token) bool {
    return self == '"';
}

inline fn isEqualSign(self: Token) bool {
    return self == '=';
}

inline fn isGreaterThan(self: Token) bool {
    return self == '>';
}

inline fn isHyphen(self: Token) bool {
    return self == '-';
}

inline fn isLeftBrace(self: Token) bool {
    return self == '{';
}

inline fn isLeftBracket(self: Token) bool {
    return self == '[';
}

inline fn isLeftParenthesis(self: Token) bool {
    return self == '(';
}

inline fn isLessThan(self: Token) bool {
    return self == '<';
}

inline fn isPlus(self: Token) bool {
    return self == '+';
}

inline fn isQuestionMark(self: Token) bool {
    return self == '?';
}

inline fn isRightBrace(self: Token) bool {
    return self == '}';
}

inline fn isRightBracket(self: Token) bool {
    return self == ']';
}

inline fn isRightParenthesis(self: Token) bool {
    return self == ')';
}

inline fn isSlash(self: Token) bool {
    return self == '/';
}

inline fn isCaret(self: Token) bool {
    return self == '^';
}

inline fn isDollar(self: Token) bool {
    return self == '$';
}

inline fn isLiteral(self: Token) bool {
    @branchHint(.likely);
    return switch (self) {
        '|', '*', '\\', ':', ',', '.', '"', '=', '>', '{', '[', '(', '<', '+', '?', '}', ']', ')' => {
            @branchHint(.unlikely);
            return false;
        },
        else => {
            @branchHint(.likely);
            return true;
        },
    };
}

fn toBindingPower(self: Token) BindingPower {
    return switch (self) {
        '|' => .alternation,
        '*' => .quantifier,
        '\\' => .escaped,
        '.' => .class,
        '"' => .quoting,
        '{' => .quantifier,
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
    const cntrl_chars = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F\x7F";
};

const Err = struct {
    TestUnexpectedParsingFailure: void,
    TestUnexpectedFormattingFailure: void,
};

fn expectAstSformExact(gpa: Allocator, pattern: []const u8, expected: []const u8) !void {
    var parser = Parser.init(gpa, pattern);
    defer parser.deinit();

    const ast = parser.parse() catch |err| {
        std.debug.print("Parsing failed for pattern \"{s}\": {s}\n", .{ pattern, @errorName(err) });
        return error.TestUnexpectedParsingFailure;
    };
    const actual = std.fmt.allocPrint(gpa, "{any}", .{ast}) catch |err| {
        std.debug.print("Formatting failed for pattern \"{s}\": {s}\n", .{ pattern, @errorName(err) });
        return error.TestUnexpectedFormattingFailure;
    };
    defer gpa.free(actual);

    try std.testing.expectEqualStrings(expected, actual);
}

test "parse single literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a", "(literal 'a')");
}

test "parse two literals concatenation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ab", "(concat (literal 'a') (literal 'b'))");
}

test "parse three literals concatenation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "abc", "(concat (concat (literal 'a') (literal 'b')) (literal 'c'))");
}

test "parse simple group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(ab)c", "(concat (group (concat (literal 'a') (literal 'b'))) (literal 'c'))");
}

test "parse group single literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)", "(group (literal 'a'))");
}

test "parse group affecting concat right" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a(bc)", "(concat (literal 'a') (group (concat (literal 'b') (literal 'c'))))");
}

test "parse concatenation of groups" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)(b)", "(concat (group (literal 'a')) (group (literal 'b')))");
}

test "parse nested groups" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "((a))", "(group (group (literal 'a')))");
}

test "parse complex groups literals mix" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a(b(c))d", "(concat (concat (literal 'a') (group (concat (literal 'b') (group (literal 'c'))))) (literal 'd'))");
}

test "parse zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a?", "(quantifier :min 0 :max 1 (literal 'a'))");
}

test "parse group zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)?", "(quantifier :min 0 :max 1 (group (literal 'a')))");
}

test "parse two group zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)?(b)?", "(concat (quantifier :min 0 :max 1 (group (literal 'a'))) (quantifier :min 0 :max 1 (group (literal 'b'))))");
}

test "parse one-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a+", "(quantifier :min 1 :max null (literal 'a'))");
}

test "parse group one-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)+", "(quantifier :min 1 :max null (group (literal 'a')))");
}

test "parse two group one-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)+(b)+", "(concat (quantifier :min 1 :max null (group (literal 'a'))) (quantifier :min 1 :max null (group (literal 'b'))))");
}

test "parse zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a*", "(quantifier :min 0 :max null (literal 'a'))");
}

test "parse group zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)*", "(quantifier :min 0 :max null (group (literal 'a')))");
}

test "parse two group zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a)*(b)*", "(concat (quantifier :min 0 :max null (group (literal 'a'))) (quantifier :min 0 :max null (group (literal 'b'))))");
}

test "parse interval simple" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{1}", "(quantifier :min 1 :max 1 (literal 'a'))");
}

test "parse interval range" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{1,2}", "(quantifier :min 1 :max 2 (literal 'a'))");
}

test "parse interval range empty" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{1,}", "(quantifier :min 1 :max null (literal 'a'))");
}

test "parse alternation simple" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|b", "(alternation (literal 'a') (literal 'b'))");
}

test "parse alternation multiple" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|b|c", "(alternation (alternation (literal 'a') (literal 'b')) (literal 'c'))");
}

test "parse alternation precedence vs concat (left)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ab|c", "(alternation (concat (literal 'a') (literal 'b')) (literal 'c'))");
}

test "parse alternation precedence vs concat (right)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|bc", "(alternation (literal 'a') (concat (literal 'b') (literal 'c')))");
}

test "parse alternation precedence vs quantifier (left)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a*|b", "(alternation (quantifier :min 0 :max null (literal 'a')) (literal 'b'))");
}

test "parse alternation precedence vs quantifier (right)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|b*", "(alternation (literal 'a') (quantifier :min 0 :max null (literal 'b')))");
}

test "parse alternation precedence multiple quantifiers" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a?|b+", "(alternation (quantifier :min 0 :max 1 (literal 'a')) (quantifier :min 1 :max null (literal 'b')))");
}

test "parse alternation inside group concatenated left" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a|b)c", "(concat (group (alternation (literal 'a') (literal 'b'))) (literal 'c'))");
}

test "parse alternation inside group concatenated right" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a(b|c)", "(concat (literal 'a') (group (alternation (literal 'b') (literal 'c'))))");
}

test "parse alternation with groups explicitly left assoc" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a|b)|c", "(alternation (group (alternation (literal 'a') (literal 'b'))) (literal 'c'))");
}

test "parse alternation with groups explicitly right assoc" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|(b|c)", "(alternation (literal 'a') (group (alternation (literal 'b') (literal 'c'))))");
}

test "parse alternation complex mix" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a*b|c?d|e", "(alternation (alternation (concat (quantifier :min 0 :max null (literal 'a')) (literal 'b')) (concat (quantifier :min 0 :max 1 (literal 'c')) (literal 'd'))) (literal 'e'))");
}

test "parse dot class literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, ".a", "(concat (class :negated #f :count 255) (literal 'a'))");
}

test "parse literal class dot" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a.", "(concat (literal 'a') (class :negated #f :count 255))");
}

test "parse class simple literals alpha" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[:alpha:]]", "(class :negated #f :count 52)");
}

test "parse class simple literals alnum" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[:alnum:]]", "(class :negated #f :count 62)");
}

test "parse class simple literals abc" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[abc]", "(class :negated #f :count 3)");
}

test "parse class simple range lowercase" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-z]", "(class :negated #f :count 26)");
}

test "parse class simple range lowercase, implicit concat" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-z]a", "(concat (class :negated #f :count 26) (literal 'a'))");
}

test "parse class negated literals" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^123]", "(class :negated #t :count 3)");
}

test "parse class literal hyphen at end" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-c-]", "(class :negated #f :count 4)");
}

test "parse escaped tab" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\t", "(literal 'tab')");
}

test "parse escaped quantifier metacharacter" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\?", "(literal '?')");
}

test "parse hex escape in concatenation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\x42c", "(concat (concat (literal 'a') (literal 'B')) (literal 'c'))");
}

test "parse octal escape quantified" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\60+", "(quantifier :min 1 :max null (literal '0'))");
}

test "parse escaped backslash in concatenation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\\\b", "(concat (concat (literal 'a') (literal 'escaped')) (literal 'b'))");
}

test "parse escapes within quantified alternation group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a\\?|b\\*)c", "(concat (group (alternation (concat (literal 'a') (literal '?')) (concat (literal 'b') (literal '*')))) (literal 'c'))");
}

test "parse escaped character with interval quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\x2A{2,3}", "(quantifier :min 2 :max 3 (literal '*'))");
}

test "parse max octal escape followed by literal digit" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\1014", "(concat (literal 'A') (literal '4'))");
}

test "parse escaped brackets and braces" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\[a\\{b\\}\\]", "(concat (concat (literal '[') (concat (literal 'a') (literal '{'))) (concat (literal 'b') (concat (literal '}') (literal ']'))))");
}

test "parse multiple mixed escapes concatenated" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\t\\x41\\\\\\(\\n", "(concat (literal 'tab') (concat (literal 'A') (concat (literal 'escaped') (concat (literal '(') (literal 'newline')))))");
}

test "parse quoted simple string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"abc\"", "(quoted abc)");
}

test "parse quoted string with metacharacters" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"a*b.\"", "(quoted a*b.)");
}

test "parse quoted empty string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"\"", "(quoted )");
}

test "parse concatenation with quoted string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\"b*c\"d", "(concat (concat (literal 'a') (quoted b*c)) (literal 'd'))");
}

test "parse escaped slash (no special handling)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\/b", "(concat (concat (literal 'a') (literal '/')) (literal 'b'))");
}

test "parse slash inside character class" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a/z]", "(class :negated #f :count 3)");
}

test "parse slash inside quoted string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"a/b\"", "(quoted a/b)");
}

test "parse character class empty" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[]");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.EmptyClass, parser.parse());
}

test "parse error unclosed parenthesis open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "(a");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnclosedParenthesis, parser.parse());
}

test "parse character class empty negated" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^]]", "(class :negated #t :count 1)");
}

test "parse character class range with escaped hex" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[\\x41-\\x43]", "(class :negated #f :count 3)");
}

test "parse character class hyphen at beginning" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[-abc]", "(class :negated #f :count 4)");
}

test "parse character class caret not at beginning" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a^bc]", "(class :negated #f :count 4)");
}

test "parse character class complex mix" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-f[:digit:]_\\t-]", "(class :negated #f :count 19)");
}

test "parse character class negated posix" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^[:digit:]]", "(class :negated #t :count 10)");
}

test "parse character class escaped special chars" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[\\^\\-\\]\\\\]", "(class :negated #f :count 4)");
}

test "parse escape at end of pattern" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a\\");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnexpectedEof, parser.parse());
}

test "parse escapes inside quoted string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"a\\nb\\tc\"", "(quoted a\\nb\\tc)");
}

test "parse interval quantifier zero min" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{0,5}", "(quantifier :min 0 :max 5 (literal 'a'))");
}

test "parse interval quantifier invalid syntax comma start" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{,5}");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse interval quantifier invalid syntax non digit" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1a}");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse interval quantifier invalid range max less than min" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{5,2}", "(quantifier :min 5 :max 2 (literal 'a'))");
}

test "parse anchor start mid pattern as literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a^b", "(concat (concat (literal 'a') (literal '^')) (literal 'b'))");
}

test "parse anchor end mid pattern as literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a$b", "(concat (concat (literal 'a') (literal '$')) (literal 'b'))");
}

test "parse error unclosed parenthesis close" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a)", "(literal 'a')");
}

test "parse error unclosed bracket open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[a");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnclosedBracket, parser.parse());
}

test "parse error unclosed brace interval open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnclosedBrace, parser.parse());
}

test "parse error unclosed brace interval comma" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1,");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse error unclosed quote" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "\"abc");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnexpectedEof, parser.parse());
}

test "parse error empty group" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "()");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.EmptyGroup, parser.parse());
}

test "parse error invalid posix class" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[[:invalid:]]");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse quantifier on quoted string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"abc\"*", "(quantifier :min 0 :max null (quoted abc))");
}

test "parse alternation with quoted string" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a|\"b*\"|c", "(alternation (alternation (literal 'a') (quoted b*)) (literal 'c'))");
}

test "parse character class caret only" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^]", "(class :negated #f :count 1)");
}

test "parse character class negated hyphen" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^-]", "(class :negated #t :count 1)");
}

test "parse character class posix inside range invalid" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-[:digit:]]", "(class :negated #f :count 13)");
}

test "parse adjacent quantifiers nesting" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a??", "(quantifier :min 0 :max 1 (quantifier :min 0 :max 1 (literal 'a')))");
    try expectAstSformExact(allocator, "a+*", "(quantifier :min 0 :max null (quantifier :min 1 :max null (literal 'a')))");
}

test "parse empty alternative start error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "|b");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse empty alternative end error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a|");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnexpectedEof, parser.parse());
}

test "parse empty alternative middle error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a||b");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse error mismatched delimiters" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "([a-z)}");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnclosedBracket, parser.parse());
}

test "parse unknown escape sequence" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\z", "(literal 'z')");
}

test "parse error syntax in interval quantifier" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1,b}");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse character class negated range" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^a-c]", "(class :negated #t :count 3)");
}

test "parse character class with escaped closing bracket" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a\\]b]", "(class :negated #f :count 3)");
}

test "parse character class invalid range z-a" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[z-a]", "(class :negated #f :count 3)");
}

test "parse escape hex incomplete 1" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "\\xG");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse escape octal incomplete" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "\\oA");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse escaped literal brace" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\{b\\}", "(concat (concat (literal 'a') (literal '{')) (concat (literal 'b') (literal '}')))");
}

test "parse quantifier on character class" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-z]+", "(quantifier :min 1 :max null (class :negated #f :count 26))");
}

test "parse quantifier on dot" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, ".?", "(quantifier :min 0 :max 1 (class :negated #f :count 255))");
}

test "parse interval quantifier zero repetition" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{0}", "(quantifier :min 0 :max 0 (literal 'a'))");
}

test "parse interval quantifier large numbers" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a{100,200}", "(quantifier :min 100 :max 200 (literal 'a'))");
    try expectAstSformExact(allocator, "b{500,}", "(quantifier :min 500 :max null (literal 'b'))");
}

test "parse quantifier on alternation operator error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a|?b");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse complex precedence mix 1" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a|b)*c{2}", "(concat (quantifier :min 0 :max null (group (alternation (literal 'a') (literal 'b')))) (quantifier :min 2 :max 2 (literal 'c')))");
}

test "parse complex precedence mix 2" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a[b-d]+|ef?", "(alternation (concat (literal 'a') (quantifier :min 1 :max null (class :negated #f :count 3))) (concat (literal 'e') (quantifier :min 0 :max 1 (literal 'f'))))");
}

test "parse escaped dot and pipe" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\.b\\|c", "(concat (concat (concat (literal 'a') (literal '.')) (concat (literal 'b') (literal '|'))) (literal 'c'))");
}

test "parse character class posix combined with range and literal hyphen" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[:digit:]a-f-]", "(class :negated #f :count 17)");
}

test "parse character class negated posix class" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^[:space:]]", "(class :negated #t :count 6)");
}

test "parse character class nested brackets literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[abc]]", "(class :negated #f :count 4)");
}

test "parse escape octal explicit length 2" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\o60", "(literal '0')");
}

test "parse escape hex mixed case" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\x4aF", "(concat (literal 'J') (literal 'F'))");
    try expectAstSformExact(allocator, "\\xAb", "(literal '\xab')");
}

test "parse error invalid character inside interval" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1,z}");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse alternation empty inside group right" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "(a|)");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.UnclosedParenthesis, parser.parse());
}

test "parse alternation empty inside group left" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "(|b)");
    defer parser.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, parser.parse());
}

test "parse quoted string followed by group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"a\"(b)", "(concat (quoted a) (group (literal 'b')))");
}

test "parse error quantifier on invalid expression start" {
    const allocator = std.testing.allocator;
    var p1 = Parser.init(allocator, "?abc");
    defer p1.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, p1.parse());

    var p2 = Parser.init(allocator, "+abc");
    defer p2.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, p2.parse());

    var p3 = Parser.init(allocator, "*abc");
    defer p3.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, p3.parse());

    var p4 = Parser.init(allocator, "{1,2}abc");
    defer p4.deinit();
    try std.testing.expectError(Parser.Error.SyntaxError, p4.parse());
}
test "auto-generated literal test 1" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a", "(literal 'a')");
}

test "auto-generated literal test 2" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "b", "(literal 'b')");
}

test "auto-generated literal test 3" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "c", "(literal 'c')");
}

test "auto-generated literal test 4" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "d", "(literal 'd')");
}

test "auto-generated literal test 5" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "e", "(literal 'e')");
}

test "auto-generated literal test 6" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "f", "(literal 'f')");
}

test "auto-generated literal test 7" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "g", "(literal 'g')");
}

test "auto-generated literal test 8" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "h", "(literal 'h')");
}

test "auto-generated literal test 9" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "i", "(literal 'i')");
}

test "auto-generated literal test 10" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "j", "(literal 'j')");
}

test "auto-generated literal test 11" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "k", "(literal 'k')");
}

test "auto-generated literal test 12" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "l", "(literal 'l')");
}

test "auto-generated literal test 13" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "m", "(literal 'm')");
}

test "auto-generated literal test 14" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "n", "(literal 'n')");
}

test "auto-generated literal test 15" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "o", "(literal 'o')");
}

test "auto-generated literal test 16" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "p", "(literal 'p')");
}

test "auto-generated literal test 17" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "q", "(literal 'q')");
}

test "auto-generated literal test 18" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "r", "(literal 'r')");
}

test "auto-generated literal test 19" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "s", "(literal 's')");
}

test "auto-generated literal test 20" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "t", "(literal 't')");
}

test "auto-generated literal test 21" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "u", "(literal 'u')");
}

test "auto-generated literal test 22" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "v", "(literal 'v')");
}

test "auto-generated literal test 23" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "w", "(literal 'w')");
}

test "auto-generated literal test 24" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "x", "(literal 'x')");
}

test "auto-generated literal test 25" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "y", "(literal 'y')");
}

test "auto-generated literal test 26" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "z", "(literal 'z')");
}

test "auto-generated literal test 27" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "A", "(literal 'A')");
}

test "auto-generated literal test 28" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "B", "(literal 'B')");
}

test "auto-generated literal test 29" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "C", "(literal 'C')");
}

test "auto-generated literal test 30" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "D", "(literal 'D')");
}

test "auto-generated literal test 31" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "E", "(literal 'E')");
}

test "auto-generated literal test 32" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "F", "(literal 'F')");
}

test "auto-generated literal test 33" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "G", "(literal 'G')");
}

test "auto-generated literal test 34" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "H", "(literal 'H')");
}

test "auto-generated literal test 35" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "I", "(literal 'I')");
}

test "auto-generated literal test 36" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "J", "(literal 'J')");
}

test "auto-generated literal test 37" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "K", "(literal 'K')");
}

test "auto-generated literal test 38" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "L", "(literal 'L')");
}

test "auto-generated literal test 39" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "M", "(literal 'M')");
}

test "auto-generated literal test 40" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "N", "(literal 'N')");
}

test "auto-generated literal test 41" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "O", "(literal 'O')");
}

test "auto-generated literal test 42" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "P", "(literal 'P')");
}

test "auto-generated literal test 43" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "Q", "(literal 'Q')");
}

test "auto-generated literal test 44" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "R", "(literal 'R')");
}

test "auto-generated literal test 45" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "S", "(literal 'S')");
}

test "auto-generated literal test 46" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "T", "(literal 'T')");
}

test "auto-generated literal test 47" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "U", "(literal 'U')");
}

test "auto-generated literal test 48" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "V", "(literal 'V')");
}

test "auto-generated literal test 49" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "W", "(literal 'W')");
}

test "auto-generated literal test 50" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "X", "(literal 'X')");
}

test "auto-generated literal test 51" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "Y", "(literal 'Y')");
}

test "auto-generated literal test 52" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "Z", "(literal 'Z')");
}

test "auto-generated literal test 53" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "0", "(literal '0')");
}

test "auto-generated literal test 54" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "1", "(literal '1')");
}

test "auto-generated literal test 55" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "2", "(literal '2')");
}

test "auto-generated literal test 56" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "3", "(literal '3')");
}

test "auto-generated literal test 57" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "4", "(literal '4')");
}

test "auto-generated literal test 58" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "5", "(literal '5')");
}

test "auto-generated literal test 59" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "6", "(literal '6')");
}

test "auto-generated literal test 60" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "7", "(literal '7')");
}

test "auto-generated literal test 61" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "8", "(literal '8')");
}

test "auto-generated literal test 62" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "9", "(literal '9')");
}

test "auto-generated literal test 63" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\!", "(literal '!')");
}

test "auto-generated literal test 64" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\\"", "(literal '\"')");
}

test "auto-generated literal test 65" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "#", "(literal '#')");
}

test "auto-generated literal test 66" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "%", "(literal '%')");
}

test "auto-generated literal test 67" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "&", "(literal '&')");
}

test "auto-generated literal test 68" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\'", "(literal 'squote')");
}

test "auto-generated literal test 69" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\,", "(literal ',')");
}

test "auto-generated literal test 70" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\-", "(literal '-')");
}

test "auto-generated literal test 71" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\/", "(literal '/')");
}

test "auto-generated literal test 72" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\:", "(literal ':')");
}

test "auto-generated literal test 73" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\;", "(literal ';')");
}

test "auto-generated literal test 74" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\<", "(literal '<')");
}

test "auto-generated literal test 75" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\=", "(literal '=')");
}

test "auto-generated literal test 76" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\>", "(literal '>')");
}

test "auto-generated literal test 77" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\@", "(literal '@')");
}

test "auto-generated literal test 78" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\_", "(literal '_')");
}

test "auto-generated literal test 79" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\`", "(literal '`')");
}

test "auto-generated literal test 80" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\~", "(literal '~')");
}

test "auto-generated literal test 81" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "aa", "(concat (literal 'a') (literal 'a'))");
}

test "auto-generated literal test 82" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "bb", "(concat (literal 'b') (literal 'b'))");
}

test "auto-generated literal test 83" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "cc", "(concat (literal 'c') (literal 'c'))");
}

test "auto-generated literal test 84" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "dd", "(concat (literal 'd') (literal 'd'))");
}

test "auto-generated literal test 85" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ee", "(concat (literal 'e') (literal 'e'))");
}

test "auto-generated literal test 86" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ff", "(concat (literal 'f') (literal 'f'))");
}

test "auto-generated literal test 87" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "gg", "(concat (literal 'g') (literal 'g'))");
}

test "auto-generated literal test 88" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "hh", "(concat (literal 'h') (literal 'h'))");
}

test "auto-generated literal test 89" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ii", "(concat (literal 'i') (literal 'i'))");
}

test "auto-generated literal test 90" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "jj", "(concat (literal 'j') (literal 'j'))");
}

test "auto-generated literal test 91" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "kk", "(concat (literal 'k') (literal 'k'))");
}

test "auto-generated literal test 92" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ll", "(concat (literal 'l') (literal 'l'))");
}

test "auto-generated literal test 93" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "mm", "(concat (literal 'm') (literal 'm'))");
}

test "auto-generated literal test 94" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "nn", "(concat (literal 'n') (literal 'n'))");
}

test "auto-generated literal test 95" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "oo", "(concat (literal 'o') (literal 'o'))");
}

test "auto-generated literal test 96" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "pp", "(concat (literal 'p') (literal 'p'))");
}

test "auto-generated literal test 97" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "qq", "(concat (literal 'q') (literal 'q'))");
}

test "auto-generated literal test 98" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "rr", "(concat (literal 'r') (literal 'r'))");
}

test "auto-generated literal test 99" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "ss", "(concat (literal 's') (literal 's'))");
}

test "auto-generated literal test 100" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "tt", "(concat (literal 't') (literal 't'))");
}

test "complex 1: alternation + quantifiers + concat" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(ab|cd)+e?f*", "(concat (concat (quantifier :min 1 :max null (group (alternation (concat (literal 'a') (literal 'b')) (concat (literal 'c') (literal 'd'))))) (quantifier :min 0 :max 1 (literal 'e'))) (quantifier :min 0 :max null (literal 'f')))");
}

test "complex 2: mixed alnum underscore class" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-zA-Z0-9_]+", "(quantifier :min 1 :max null (class :negated #f :count 63))");
}

test "complex 3: grouped alternation interval" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(foo|bar){2,3}", "(quantifier :min 2 :max 3 (group (alternation (concat (concat (literal 'f') (literal 'o')) (literal 'o')) (concat (concat (literal 'b') (literal 'a')) (literal 'r')))))");
}

test "complex 4: repeated digit‑dash group then final digits" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "([[:digit:]]{2}-){3}[[:digit:]]{4}", "(concat (quantifier :min 3 :max 3 (group (concat (quantifier :min 2 :max 2 (class :negated #f :count 10)) (literal '-')))) (quantifier :min 4 :max 4 (class :negated #f :count 10)))");
}

test "complex 5: quoted literal plus quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"hello*\"+", "(quantifier :min 1 :max null (quoted hello*))");
}

test "complex 6: digit literal + dot + digit*" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\d+\\.\\d*", "(concat (quantifier :min 1 :max null (literal 'd')) (concat (literal '.') (quantifier :min 0 :max null (literal 'd'))))");
}

test "complex 7: negated class + interval + optional xyz" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^a-z]{1,}(xyz)?", "(concat (quantifier :min 1 :max null (class :negated #t :count 26)) (quantifier :min 0 :max 1 (group (concat (concat (literal 'x') (literal 'y')) (literal 'z')))))");
}

test "complex 8: nested alternation inside group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a(b|c(d|e)f)g", "(concat (concat (literal 'a') (group (alternation (literal 'b') (concat (concat (literal 'c') (group (alternation (literal 'd') (literal 'e')))) (literal 'f'))))) (literal 'g'))");
}

test "complex 9: escaped hyphen and pipe in repeated group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "([abc]\\-\\|){2}", "(quantifier :min 2 :max 2 (group (concat (class :negated #f :count 3) (concat (literal '-') (literal '|')))))");
}

test "complex 10: hex, class range, optional literal" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\x41[\\x42-\\x44]\\d?", "(concat (literal 'A') (concat (class :negated #f :count 3) (quantifier :min 0 :max 1 (literal 'd'))))");
}

test "complex 11: alpha+ underscore class, then w{3}" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[:alpha:]_]+w{3}", "(concat (quantifier :min 1 :max null (class :negated #f :count 53)) (quantifier :min 3 :max 3 (literal 'w')))");
}

test "complex 12: dot+ then dot+? nested" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, ".+?", "(quantifier :min 0 :max 1 (quantifier :min 1 :max null (class :negated #f :count 255)))");
}

test "complex 13: multiple groups with star, plus, question" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a|b)(c|d)*e+f?", "(concat (concat (concat (group (alternation (literal 'a') (literal 'b'))) (quantifier :min 0 :max null (group (alternation (literal 'c') (literal 'd'))))) (quantifier :min 1 :max null (literal 'e'))) (quantifier :min 0 :max 1 (literal 'f')))");
}

test "complex 14: class ranges and alternation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-cx-z]{2,4}y|z", "(alternation (concat (quantifier :min 2 :max 4 (class :negated #f :count 6)) (literal 'y')) (literal 'z'))");
}

test "complex 15: abc(def){0,1}ghi*" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "abc(def){0,1}ghi*", "(concat (concat (concat (concat (concat (literal 'a') (literal 'b')) (concat (literal 'c') (quantifier :min 0 :max 1 (group (concat (concat (literal 'd') (literal 'e')) (literal 'f')))))) (literal 'g')) (literal 'h')) (quantifier :min 0 :max null (literal 'i')))");
}

test "complex 16: uppercase class + literal d repeated" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "([A-Z]\\d){3,}", "(quantifier :min 3 :max null (group (concat (class :negated #f :count 26) (literal 'd'))))");
}

test "complex 17: chained alternation with interval" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "xy|zw|pq{2}", "(alternation (alternation (concat (literal 'x') (literal 'y')) (concat (literal 'z') (literal 'w'))) (concat (literal 'p') (quantifier :min 2 :max 2 (literal 'q'))))");
}

test "complex 18: two empty‑negation classes concat" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[^][^]", "(concat (class :negated #f :count 1) (class :negated #f :count 1))");
}

test "complex 19: class with escaped hyphen" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a\\-b]", "(class :negated #f :count 3)");
}

test "complex 20: quoted regex metachars" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"(.*?)\"", "(quoted (.*?))");
}

test "lex complex 1: nested quantifiers on group" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(ab)*+", "(quantifier :min 1 :max null (quantifier :min 0 :max null (group (concat (literal 'a') (literal 'b')))))");
}

test "lex complex 2: literal caret and dollar" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "^$", "(concat (literal '^') (literal '$'))");
}

test "lex complex 3: escaped caret inside concatenation" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a\\^b", "(concat (concat (literal 'a') (literal '^')) (literal 'b'))");
}

test "lex complex 4: hex + and octal escapes with quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\\x41+\\101", "(concat (quantifier :min 1 :max null (literal 'A')) (literal 'A'))");
}

test "lex complex 5: combined POSIX class and literal hyphen" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[:digit:]-a]", "(class :negated #f :count 12)");
}

test "lex complex 6: nested character-class union (POSIX‑correct)" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[[a-c][d-f]]", "(concat (class :negated #f :count 4) (class :negated #f :count 3))");
}

test "lex complex 7: quoted regex metacharacters" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "\"(a|b)*\"", "(quoted (a|b)*)");
}

test "lex complex 8: nested group and interval quantifier" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "(a(b(c){1,2})+)?", "(quantifier :min 0 :max 1 (group (concat (literal 'a') (quantifier :min 1 :max null (group (concat (literal 'b') (quantifier :min 1 :max 2 (group (literal 'c')))))))))");
}

test "lex complex 9: alternation with mixed quantifiers" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "abc|def+ghi?", "(alternation (concat (concat (literal 'a') (literal 'b')) (literal 'c')) (concat (concat (concat (concat (concat (literal 'd') (literal 'e')) (quantifier :min 1 :max null (literal 'f'))) (literal 'g')) (literal 'h')) (quantifier :min 0 :max 1 (literal 'i'))))");
}

test "lex complex 10: class plus group interval" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "[a-c]+(x|y){3,}", "(concat (quantifier :min 1 :max null (class :negated #f :count 3)) (quantifier :min 3 :max null (group (alternation (literal 'x') (literal 'y')))))");
}

test "lex complex 11: super long mixed regex" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "([[:upper:]]+[[:digit:]]{3}|foo_bar){2}[[:alnum:]_-]+", "(concat (quantifier :min 2 :max 2 (group (alternation (concat (quantifier :min 1 :max null (class :negated #f :count 26)) (quantifier :min 3 :max 3 (class :negated #f :count 10))) (concat (concat (concat (concat (concat (concat (literal 'f') (literal 'o')) (literal 'o')) (literal '_')) (literal 'b')) (literal 'a')) (literal 'r'))))) (quantifier :min 1 :max null (class :negated #f :count 64)))");
}
