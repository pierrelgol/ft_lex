const std = @import("std");
const mem = std.mem;
const math = std.math;
const Order = math.Order;
const assert = std.debug.assert;
const Allocator = mem.Allocator;
const EnumMap = std.EnumMap;
const Ast = @import("Ast.zig").Ast;
const AstNode = Ast.Node;
const AstNodeKind = AstNode.Kind;
const Lexer = @import("Lexer.zig");
const LexerMode = Lexer.Mode;
const Token = @import("Token.zig").Token;
const TokenKind = Token.Kind;

const Charset = std.StaticBitSet(std.math.maxInt(u8));

pub const Parser = struct {
    lex: Lexer,
    ast: Ast,
    cur: Token,
    bpw: EnumMap(TokenKind, BindingPower),
    nud: EnumMap(TokenKind, nudFn),
    led: EnumMap(TokenKind, ledFn),
    depth: usize,
    trailing: bool,

    pub const ParsingError = error{
        UnexpectedEof,
        UnclosedParenthesis,
        NoNudFunction,
        NoLedFunction,
        SyntaxError,
        InvalidRange,
    } || Allocator.Error;

    pub const nudFn: type = *const fn (*Parser) ParsingError!*AstNode;
    pub const ledFn: type = *const fn (*Parser, *AstNode) ParsingError!*AstNode;

    pub fn init(gpa: Allocator, pattern: []const u8) Parser {
        var lexer = Lexer.init(pattern);
        return .{
            .lex = lexer,
            .ast = Ast.init(gpa),
            .cur = lexer.curr(),
            .bpw = binding_power_map,
            .nud = nud_map,
            .led = led_map,
            .depth = 0,
            .trailing = false,
        };
    }

    pub fn deinit(self: *Parser) void {
        defer self.* = undefined;
        self.ast.deinit();
    }

    pub fn current(self: *const Parser) Token {
        return self.cur;
    }

    pub fn increaseDepth(self: *Parser) void {
        self.depth += 1;
    }

    pub fn decreaseDepth(self: *Parser) void {
        self.depth -= 1;
    }

    pub fn advance(self: *Parser) Token {
        return token: {
            const value = self.cur;
            self.cur = self.lex.next();
            break :token value;
        };
    }

    pub fn eat(self: *Parser) *Parser {
        self.cur = self.lex.next();
        return self;
    }

    pub fn next(self: *Parser) Token {
        return self.advance();
    }

    pub fn peek(self: *Parser) Token {
        return self.lex.peek();
    }

    pub fn setMode(self: *Parser, mode: LexerMode) void {
        self.lex.setMode(mode);
    }

    pub fn getMode(self: *Parser) LexerMode {
        return self.lex.getMode();
    }

    pub fn createNode(self: *Parser, init_expr: AstNode) ParsingError!*AstNode {
        const node = try self.ast.createNode();
        node.* = init_expr;
        return node;
    }

    pub fn compareBindingPower(self: *Parser, bp: BindingPower) Order {
        const cur_bp: u8 = @intFromEnum(self.getBindingPower());
        const min_bp: u8 = @intFromEnum(bp);
        return if (cur_bp < min_bp) .lt else if (cur_bp > min_bp) .gt else .eq;
    }

    pub fn parseRegexPattern(self: *Parser) ParsingError!Ast {
        return self.ast;
    }

    pub fn parseExpression(self: *Parser, min_bp: BindingPower) ParsingError!*AstNode {
        var left = try self.parseNud();
        while (self.current().tag() != .eof and self.compareBindingPower(min_bp) != .lt) : (left = try self.parseLed(left)) {}
        return left;
    }

    pub fn parseLed(self: *Parser, lhs: *AstNode) ParsingError!*AstNode {
        if (self.led.get(self.current().tag())) |led_fn| {
            return led_fn(self, lhs);
        } else {
            return error.NoLedFunction;
        }
    }

    pub fn parseNud(self: *Parser) ParsingError!*AstNode {
        if (self.nud.get(self.current().tag())) |nud_fn| {
            return nud_fn(self);
        } else {
            return error.NoNudFunction;
        }
    }

    pub fn getBindingPower(self: *const Parser) BindingPower {
        return self.bpw.getAssertContains(self.current().tag());
    }

    pub const BindingPower = enum(u8) {
        none = 0,
        alternation = 1,
        interval_expr = 2,
        concatenation = 3,
        quantifier = 4,
        definition = 5,
        grouping = 6,
        bracket_expr = 7,
        escaped_char = 8,
        collating_expr = 9,
    };

    pub const binding_power_map: EnumMap(TokenKind, BindingPower) = .initFullWith(.{
        .alternation = .alternation,
        .anchor_end = .none,
        .anchor_start = .none,
        .asterisk = .quantifier,
        .backslash = .escaped_char,
        .dot = .concatenation,
        .double_quote = .definition,
        .eof = .none,
        .left_brace = .interval_expr,
        .left_bracket = .bracket_expr,
        .left_parenthesis = .grouping,
        .literal = .concatenation,
        .plus = .quantifier,
        .question_mark = .quantifier,
        .right_brace = .none,
        .right_bracket = .none,
        .right_parenthesis = .none,
        .trailing_context = .none,
    });

    pub const nud_map: EnumMap(TokenKind, nudFn) = .initFullWith(.{
        .left_bracket = nudBracket,
        .literal = nudLiteral,
        .dot = nudBracket,
        .anchor_start = nudAnchorStart,
        .left_parenthesis = nudGroup,
        .backslash = nudEscape,

        .left_brace = nudError,
        .alternation = nudError,
        .anchor_end = nudError,
        .asterisk = nudError,
        .double_quote = nudError,
        .eof = nudError,
        .plus = nudError,
        .question_mark = nudError,
        .right_brace = nudError,
        .right_bracket = nudError,
        .right_parenthesis = nudError,
        .trailing_context = nudError,
    });

    pub fn nudError(self: *Parser) ParsingError!*AstNode {
        _ = self;
        return error.NoNudFunction;
    }

    pub fn nudLiteral(self: *Parser) ParsingError!*AstNode {
        assert(self.current().tag() == .literal);
        return self.createNode(.{ .literal = self.advance().literal });
    }

    pub fn nudEscape(self: *Parser) ParsingError!*AstNode {
        assert(self.current().tag() == .backslash);
        assert(self.getMode() == .regex);
        self.eat().setMode(.bracket);

        if (self.current().tag() == .eof)
            return error.UnexpectedEof;

        defer self.eat().setMode(.regex);
        return self.createNode(.{ .literal = self.current().literal });
    }

    pub fn nudGroup(self: *Parser) ParsingError!*AstNode {
        assert(self.current().tag() == .left_parenthesis);
        self.eat().increaseDepth();
        const group = try self.parseExpression(.none);
        if (self.current().tag() != .right_parenthesis) {
            self.decreaseDepth();
            return error.UnclosedParenthesis;
        }
        self.eat().decreaseDepth();
        return self.createNode(.{ .grouping = group });
    }

    pub fn nudAnchorStart(self: *Parser) ParsingError!*AstNode {
        assert(self.current().tag() == .anchor_start);
        return self.eat().createNode(.{ .anchor_start = try self.parseExpression(.none) });
    }

    fn nudBracketPosix(self: *Parser) ParsingError!PosixClass {
        const Lbracket = Token.init(.literal, '[');
        const Colon = Token.init(.literal, ':');
        const Rbracket = Token.init(.literal, ']');
        assert(self.getMode() == .bracket);
        assert(self.current().eql(Lbracket));
        assert(self.peek().eql(Colon));
        _ = self.eat().eat();
        var buffer = std.BoundedArray(u8, 256).init(0) catch unreachable;
        while (true) : (buffer.appendAssumeCapacity(self.current().literal)) {
            if (self.current().eql(Colon) and self.peek().eql(Rbracket)) {
                _ = self.eat().eat();
                break;
            }
        }
        return std.meta.stringToEnum(PosixClass, buffer.constSlice()) orelse error.SyntaxError;
    }

    pub fn nudBracket(self: *Parser) ParsingError!*AstNode {
        assert(self.getMode() == .regex);

        if (self.current().tag() == .dot) {
            _ = self.advance();
            var charset = Charset.initFull();
            charset.setValue(0, false);
            return self.createNode(.{ .class = .{ .negated = false, .charset = charset } });
        } else {
            assert(self.current().tag() == .left_bracket);
            self.eat().setMode(.bracket);

            var charset = Charset.initEmpty();
            var negated: bool = false;

            if (self.current().tag() == .literal and self.current().literal == '^') {
                negated = true;
                _ = self.eat();
            }

            if (self.current().tag() == .literal and (self.current().literal == ']' or self.current().literal == '-')) {
                charset.set(self.current().literal);
                _ = self.eat();
            }

            while (true) {
                if (self.current().tag() == .eof) return error.UnexpectedEof;

                if (self.current().tag() == .literal and self.current().literal == ']') {
                    break;
                }

                if (self.current().tag() == .literal and self.current().literal == '[' and
                    self.peek().tag() == .literal and self.peek().literal == ':')
                {
                    const posix_class = try self.nudBracketPosix();
                    const posix_charset = posix_class.toCharset();
                    charset = charset.unionWith(posix_charset);
                    continue;
                }

                if (self.current().tag() == .literal and
                    self.peek().tag() == .literal and self.peek().literal == '-')
                {
                    const range_start = self.current().literal;
                    _ = self.eat();
                    _ = self.eat();

                    if (self.current().tag() == .literal and self.current().literal == ']') {
                        charset.set(range_start);
                        charset.set('-');
                    } else if (self.current().tag() == .literal) {
                        const range_end = self.current().literal;
                        if (range_end < range_start) {
                            return error.InvalidRange;
                        } else {
                            charset.setRangeValue(.{ .start = range_start, .end = range_end }, true);
                        }
                        _ = self.eat();
                        continue;
                    } else {
                        return error.SyntaxError;
                    }
                } else if (self.current().tag() == .literal) {
                    charset.set(self.current().literal);
                    _ = self.eat();
                } else {
                    return error.SyntaxError;
                }
            }

            assert(self.current().tag() == .literal and self.current().literal == ']');
            self.eat().setMode(.regex);

            return self.createNode(.{ .class = .{ .negated = negated, .charset = charset } });
        }
    }

    pub const led_map: EnumMap(TokenKind, ledFn) = .initFullWith(.{
        .asterisk = ledQuantifier,
        .plus = ledQuantifier,
        .left_brace = ledQuantifier,
        .left_bracket = ledConcatenation,
        .left_parenthesis = ledConcatenation,
        .question_mark = ledQuantifier,
        .alternation = ledAlternation,
        .dot = ledConcatenation,
        .literal = ledConcatenation,
        .backslash = ledConcatenation,
        .anchor_end = ledAnchorEnd,
        .trailing_context = ledTrailingContext,
        .double_quote = ledConcatenation,

        .anchor_start = ledError,
        .eof = ledError,
        .right_brace = ledError,
        .right_bracket = ledError,
        .right_parenthesis = ledError,
    });

    pub fn ledConcatenation(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        return self.createNode(.{
            .concatenation = .{
                .lhs = left,
                .rhs = try self.parseExpression(.concatenation),
            },
        });
    }

    pub fn ledAlternation(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        _ = self.eat();
        return self.createNode(.{
            .alternation = .{
                .lhs = left,
                .rhs = try self.parseExpression(.concatenation),
            },
        });
    }

    pub fn ledAnchorEnd(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        _ = self.eat();
        return self.createNode(.{
            .anchor_end = left,
        });
    }

    pub fn ledTrailingContext(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        if (self.trailing) return error.SyntaxError else self.trailing = true;
        return self.eat().createNode(.{
            .trailing_context = .{
                .lhs = left,
                .rhs = try self.parseExpression(.none),
            },
        });
    }

    fn ledBraceParseInt(self: *Parser) ParsingError!usize {
        var buffer = std.BoundedArray(u8, 256).init(0) catch unreachable;
        while (true) : (buffer.appendAssumeCapacity(self.next().literal)) {
            if (self.current().eql(.{ .literal = ',' }) or self.current().eql(.{ .right_brace = {} })) {
                break;
            }

            if (self.current().tag() != .literal or !PosixClass.isDigit(self.current().literal)) {
                return error.SyntaxError;
            }
        }
        return std.fmt.parseUnsigned(u32, buffer.constSlice(), 10) catch error.SyntaxError;
    }

    pub fn ledBraceExpression(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        const min: usize = try self.ledBraceParseInt();

        if (self.current().eql(.{ .right_brace = {} })) {
            return self.eat().createNode(.{ .quantifier = .{
                .lhs = left,
                .min = min,
                .max = min,
                .greedy = false,
            } });
        }

        if (self.current().tag() != .literal or self.current().literal != ',') {
            return error.SyntaxError;
        }

        if (self.eat().current().eql(.{ .right_brace = {} })) {
            return self.eat().createNode(.{ .quantifier = .{
                .lhs = left,
                .min = min,
                .max = null,
                .greedy = false,
            } });
        }

        const max: usize = try self.ledBraceParseInt();

        assert(self.current().eql(.{ .right_brace = {} }));
        return self.eat().createNode(.{ .quantifier = .{
            .lhs = left,
            .min = min,
            .max = max,
            .greedy = false,
        } });
    }

    pub fn ledQuantifier(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        return switch (self.advance()) {
            .asterisk => self.createNode(.{ .quantifier = .{
                .min = 0,
                .max = null,
                .lhs = left,
                .greedy = false,
            } }),
            .plus => self.createNode(.{ .quantifier = .{
                .min = 1,
                .max = null,
                .lhs = left,
                .greedy = false,
            } }),
            .question_mark => self.createNode(.{ .quantifier = .{
                .min = 0,
                .max = 1,
                .lhs = left,
                .greedy = false,
            } }),
            .left_brace => self.ledBraceExpression(left),
            else => unreachable,
        };
    }

    pub fn ledError(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        _ = self;
        _ = left;
        return error.NoLedFunction;
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

    fn isUpper(c: u8) bool {
        return c >= 'A' and c <= 'Z';
    }

    fn isLower(c: u8) bool {
        return c >= 'a' and c <= 'z';
    }

    fn isAlpha(c: u8) bool {
        return (c | 32) >= 'a' and (c | 32) <= 'z';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isHex(c: u8) bool {
        return (c >= '0' and c <= '9') or ((c | 32) >= 'a' and (c | 32) <= 'f');
    }

    fn isAlnum(c: u8) bool {
        return ((c | 32) >= 'a' and (c | 32) <= 'z') or (c >= '0' and c <= '9');
    }

    fn isPunct(c: u8) bool {
        return !isAlnum(c) and isPrint(c);
    }

    fn isPrint(c: u8) bool {
        return isAscii(c) and !isCntrl(c);
    }

    fn isGraph(c: u8) bool {
        return isPrint(c) and c != ' ';
    }

    fn isSpace(c: u8) bool {
        return switch (c) {
            ' ', '\t'...'\r' => true,
            else => false,
        };
    }

    fn isBlank(c: u8) bool {
        return c == ' ' or c == '\t';
    }

    fn isAscii(c: u8) bool {
        return c < 128;
    }

    fn isCntrl(c: u8) bool {
        return c <= std.ascii.control_code.us or c == std.ascii.control_code.del;
    }

    pub const Predicate: type = *const fn (u8) bool;

    pub fn toCharset(class: PosixClass) Charset {
        const predicate: Predicate = switch (class) {
            .upper => PosixClass.isUpper,
            .lower => PosixClass.isLower,
            .alpha => PosixClass.isAlpha,
            .digit => PosixClass.isDigit,
            .alnum => PosixClass.isAlnum,
            .punct => PosixClass.isPunct,
            .blank => PosixClass.isBlank,
            .space => PosixClass.isSpace,
            .cntrl => PosixClass.isCntrl,
            .graph => PosixClass.isGraph,
            .print => PosixClass.isPrint,
            .xdigit => PosixClass.isHex,
        };
        var charset = Charset.initEmpty();
        for (0..std.math.maxInt(u8)) |c| {
            charset.setValue(c, predicate(@as(u8, @intCast(c))));
        }
        return charset;
    }
};
