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

pub const Parser = struct {
    lexer: Lexer,
    ast: Ast,
    current: Token,

    pub const ParsingError = error{
        UnexpectedEof,
        UnclosedParenthesis,
        UnclosedBracket,
        UnclosedBrace,
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
            .lexer = lexer,
            .ast = Ast.init(gpa),
            .cur = lexer.curr(),
        };
    }

    pub fn deinit(self: *Parser) void {
        defer self.* = undefined;
        self.ast.deinit();
    }

    pub fn next(self: *Parser) Token {
        const value = self.current;
        self.current = self.lexer.next();
        return value;
    }

    pub fn peek(self: *Parser) Token {
        return self.lexer.peek();
    }

    pub fn setMode(self: *Parser, mode: LexerMode) void {
        self.lexer.setMode(mode);
    }

    pub fn createNode(self: *Parser, init_expr: AstNode) ParsingError!*AstNode {
        const node = try self.ast.createNode();
        node.* = init_expr;
        return node;
    }

    pub fn parse(self: *Parser) ParsingError!Ast {
        self.ast.root = try self.parseExpression(.none);
        return self.ast;
    }

    pub fn parseExpression(self: *Parser, min_bp: BindingPower) ParsingError!*AstNode {
        var left = try self.parseNud();
        while (self.current.tag() != .eof) {
            if (self.cmpBindingPower(min_bp) == .lt)
                break
            else
                left = try self.parseLed(left);
        }
        return left;
    }

    pub fn parseLed(self: *Parser, lhs: *AstNode) ParsingError!*AstNode {
        if (led_map.get(self.current.tag())) |led_fn| {
            return led_fn(self, lhs);
        } else {
            return error.NoLedFunction;
        }
    }

    pub fn parseNud(self: *Parser) ParsingError!*AstNode {
        if (nud_map.get(self.current.tag())) |nud_fn| {
            return nud_fn(self);
        } else {
            return error.NoNudFunction;
        }
    }

    pub fn getBindingPower(self: *const Parser) BindingPower {
        return binding_power_map.getAssertContains(self.current().tag());
    }

    pub fn cmpBindingPower(self: *Parser, bp: BindingPower) Order {
        const cur_bp: u8 = @intFromEnum(self.getBindingPower());
        const min_bp: u8 = @intFromEnum(bp);
        return if (cur_bp < min_bp) .lt else if (cur_bp > min_bp) .gt else .eq;
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

    pub fn nudError(self: *Parser) ParsingError!*AstNode {
        _ = self;
        return error.NoNudFunction;
    }

    pub fn nudLiteral(self: *Parser) ParsingError!*AstNode {
        assert(self.current().tag() == .literal);
        return self.createNode(.{ .literal = self.next().literal });
    }

    pub const nud_map: EnumMap(TokenKind, nudFn) = .initFullWith(.{
        .alternation = nudError,
        .anchor_end = nudError,
        .anchor_start = nudError,
        .asterisk = nudError,
        .backslash = nudError,
        .dot = nudError,
        .double_quote = nudError,
        .eof = nudError,
        .left_brace = nudError,
        .left_bracket = nudError,
        .left_parenthesis = nudError,
        .literal = nudError,
        .plus = nudError,
        .question_mark = nudError,
        .right_brace = nudError,
        .right_bracket = nudError,
        .right_parenthesis = nudError,
        .trailing_context = nudError,
    });

    pub const led_map: EnumMap(TokenKind, ledFn) = .initFullWith(.{
        .alternation = ledError,
        .anchor_end = ledError,
        .anchor_start = ledError,
        .asterisk = ledError,
        .backslash = ledError,
        .dot = ledError,
        .double_quote = ledError,
        .eof = ledError,
        .left_brace = ledError,
        .left_bracket = ledError,
        .left_parenthesis = ledError,
        .literal = ledError,
        .plus = ledError,
        .question_mark = ledError,
        .right_brace = ledError,
        .right_bracket = ledError,
        .right_parenthesis = ledError,
        .trailing_context = ledError,
    });

    pub fn ledError(self: *Parser, left: *AstNode) ParsingError!*AstNode {
        _ = self;
        _ = left;
        return error.NoLedFunction;
    }
};
