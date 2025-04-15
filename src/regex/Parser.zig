const std = @import("std");
const mem = std.mem;
const math = std.math;
const Order = math.Order;
const assert = std.debug.assert;
const Allocator = mem.Allocator;

const Ast = @import("Ast.zig").Ast;
const Node = Ast.Node;
const Lexer = @import("Lexer.zig").Lexer;
const Token = @import("Token.zig").Token;

const Charset = std.bit_set.StaticBitSet(256);

pub const Parser = struct {
    lexer: Lexer,
    token: Token,
    ast: Ast,

    pub fn init(gpa: Allocator, pattern: []const u8) Parser {
        var lexer: Lexer = .init(pattern);
        const initial_token = lexer.next();
        return .{
            .lexer = lexer,
            .token = initial_token,
            .ast = Ast.init(gpa),
        };
    }

    pub fn deinit(self: *Parser) void {
        defer self.* = undefined;
        self.ast.deinit();
    }

    pub const ParseError = error{
        NudNotImplemented,
        LedNotImplemented,
        UnexpectedEOF,
        UnclosedQuote,
        UnclosedParenthesis,
        UnclosedBracket,
        UnclosedBrace,
        UnclosedCondition,
        UnclosedIdentifier,
        EmptyGroup,
        SyntaxError,
    } || Allocator.Error;

    fn eats(self: *Parser, n: usize) void {
        assert(n > 0);
        for (0..n) |_| {
            _ = self.next();
        }
    }

    fn eatsLiteral(self: *Parser, n: usize) void {
        assert(n > 0);
        for (0..n) |_| {
            _ = self.nextLiteral();
        }
    }

    fn next(self: *Parser) Token {
        const prev_token = self.token;
        self.token = self.lexer.next();
        return prev_token;
    }

    fn nextLiteral(self: *Parser) Token {
        const prev_token = self.token.toLiteral();
        self.token = self.lexer.next();
        return prev_token;
    }

    fn peek(self: *Parser) Token {
        return self.lexer.peek();
    }

    fn peekLiteral(self: *Parser) Token {
        var token = self.lexer.peek();
        token = token.toLiteral();
        return token;
    }

    fn curr(self: *Parser) Token {
        return self.token;
    }

    fn createNode(self: *Parser, init_expr: Node) Allocator.Error!*Node {
        const node = try self.ast.createNode();
        node.* = init_expr;
        return node;
    }

    pub fn parse(self: *Parser) ParseError!Ast {
        if (self.token.kind() == .eof) {
            self.ast.root = null;
        } else {
            self.ast.root = try self.parseExpression(.none);
        }
        return self.ast;
    }

    fn parseExpression(self: *Parser, bp: BindingPower) ParseError!*Node {
        var left: *Node = try self.nud();

        while (self.token.kind() != .eof) {
            if (self.compareBindingPower(bp) == .gt)
                left = try self.led(left)
            else
                break;
        }
        return left;
    }

    fn nud(self: *Parser) ParseError!*Node {
        const current_token = self.token;
        return switch (current_token.kind()) {
            .literal => self.nudLiteral(),
            .left_parenthesis => self.nudGroup(),
            .dot => self.nudDot(),
            .left_bracket => self.nudClass(),
            .eof => error.UnexpectedEOF,
            else => error.NudNotImplemented,
        };
    }

    fn led(self: *Parser, left: *Node) ParseError!*Node {
        const current_token = self.token;
        return switch (current_token.kind()) {
            .literal => self.ledLiteral(left),
            .left_parenthesis => self.ledImplicitConcat(left),
            .question_mark => self.ledZeroOrOne(left),
            .plus => self.ledOneOrMore(left),
            .dot => self.ledImplicitConcat(left),
            .left_bracket => self.ledImplicitConcat(left),
            .asterisk => self.ledZeroOrMore(left),
            .left_brace => self.ledIntervalExpression(left),
            .alternation => self.ledAlternation(left),
            .eof => error.UnexpectedEOF,
            else => error.LedNotImplemented,
        };
    }

    fn nudLiteral(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .literal);
        const literal_value = self.token.literal;
        const node = try self.createNode(.{
            .literal = literal_value,
        });
        self.eats(1);
        return node;
    }

    fn nudGroup(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .left_parenthesis);
        self.eats(1);
        if (self.token.kind() == .right_parenthesis) {
            return error.EmptyGroup;
        }
        const group = try self.parseExpression(.none);
        assert(self.token.kind() == .right_parenthesis);
        self.eats(1);

        return self.createNode(.{ .group = group });
    }

    fn nudDot(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .dot);
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

    fn ledLiteral(self: *Parser, left: *Node) ParseError!*Node {
        return self.createNode(.{
            .concat = .{
                .lhs = left,
                .rhs = try self.parseExpression(.concatenation),
            },
        });
    }

    fn ledImplicitConcat(self: *Parser, left: *Node) ParseError!*Node {
        return self.createNode(.{
            .concat = .{
                .lhs = left,
                .rhs = try self.parseExpression(.concatenation),
            },
        });
    }

    fn ledZeroOrOne(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .question_mark);
        self.eats(1);
        return self.createNode(.{
            .quantifier = .{
                .min = 0,
                .max = 1,
                .lhs = left,
            },
        });
    }

    fn ledZeroOrMore(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .asterisk);
        self.eats(1);
        return self.createNode(.{
            .quantifier = .{
                .min = 0,
                .max = null,
                .lhs = left,
            },
        });
    }

    fn ledOneOrMore(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .plus);
        self.eats(1);
        return self.createNode(.{
            .quantifier = .{
                .min = 1,
                .max = null,
                .lhs = left,
            },
        });
    }

    const usize_digits_length: usize = std.math.log10_int(@as(usize, std.math.maxInt(usize)));

    fn parseIntervalNumber(self: *Parser) ParseError!usize {
        if (self.token.kind() != .literal or !std.ascii.isDigit(self.token.literal)) {
            return error.SyntaxError;
        }

        var buffer = std.BoundedArray(u8, usize_digits_length).init(0) catch unreachable;

        while (true) {
            switch (self.token) {
                .literal => |c| switch (c) {
                    '0'...'9' => buffer.appendAssumeCapacity(self.next().literal),
                    ',', '}' => break,
                    else => break,
                },
                .comma, .right_brace => break,
                else => return error.SyntaxError,
            }
        }

        return std.fmt.parseUnsigned(usize, buffer.constSlice(), 10) catch {
            return error.SyntaxError;
        };
    }

    fn ledIntervalExpression(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .left_brace);
        self.eats(1);

        const min = try self.parseIntervalNumber();

        if (self.token.kind() == .right_brace) {
            self.eats(1);
            return self.createNode(.{
                .quantifier = .{
                    .min = min,
                    .max = min,
                    .lhs = left,
                },
            });
        }

        if (self.token.kind() != .comma) {
            return error.SyntaxError;
        } else {
            self.eats(1);
        }

        if (self.token.kind() == .right_brace) {
            return self.createNode(.{
                .quantifier = .{
                    .min = min,
                    .max = null,
                    .lhs = left,
                },
            });
        }

        const max = try self.parseIntervalNumber();

        if (self.token.kind() != .right_brace) {
            return error.SyntaxError;
        } else {
            self.eats(1);
        }

        return self.createNode(.{
            .quantifier = .{
                .min = min,
                .max = max,
                .lhs = left,
            },
        });
    }

    fn ledAlternation(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .alternation);
        self.eats(1);
        return self.createNode(.{
            .alternation = .{
                .lhs = left,
                .rhs = try self.parseExpression(.alternation),
            },
        });
    }

    fn compareBindingPower(self: *Parser, bp: BindingPower) Order {
        const current_token_kind = self.token.kind();
        const current_bp_value = switch (current_token_kind) {
            .literal => @intFromEnum(BindingPower.concatenation),
            .alternation => @intFromEnum(BindingPower.alternation),
            .question_mark => @intFromEnum(BindingPower.quantifier),
            .plus => @intFromEnum(BindingPower.quantifier),
            .asterisk => @intFromEnum(BindingPower.quantifier),
            .left_parenthesis => @intFromEnum(BindingPower.grouping),
            .right_parenthesis => @intFromEnum(BindingPower.none),
            .left_brace => @intFromEnum(BindingPower.interval_expression),
            .right_brace => @intFromEnum(BindingPower.none),
            .left_bracket => @intFromEnum(BindingPower.class),
            .right_bracket => @intFromEnum(BindingPower.none),
            .dot => @intFromEnum(BindingPower.class),
            else => @intFromEnum(BindingPower.none),
        };
        const min_bp_value: u8 = @intFromEnum(bp);
        return math.order(current_bp_value, min_bp_value);
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
        bracket = 10,
    };
};

fn expectAstSformExact(gpa: Allocator, pattern: []const u8, expected: []const u8) !void {
    var parser: Parser = .init(gpa, pattern);
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
    const pattern: []const u8 = "a";
    const expected: []const u8 = "(literal 'a')";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse two literals concatenation" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "ab";
    const expected: []const u8 = "(concat (literal 'a') (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse three literals concatenation" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "abc";
    const expected: []const u8 = "(concat (concat (literal 'a') (literal 'b')) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse simple group" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(ab)c";
    const expected: []const u8 = "(concat (group (concat (literal 'a') (literal 'b'))) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse group single literal" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)";
    const expected: []const u8 = "(group (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse group affecting concat right" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a(bc)";
    const expected: []const u8 = "(concat (literal 'a') (group (concat (literal 'b') (literal 'c'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse concatenation of groups" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)(b)";
    const expected: []const u8 = "(concat (group (literal 'a')) (group (literal 'b')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse nested groups" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "((a))";
    const expected: []const u8 = "(group (group (literal 'a')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse complex groups literals mix" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a(b(c))d";
    const expected: []const u8 = "(concat (concat (literal 'a') (group (concat (literal 'b') (group (literal 'c'))))) (literal 'd'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a?";
    const expected: []const u8 = "(quantifier :min 0 :max 1 (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse group zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)?";
    const expected: []const u8 = "(quantifier :min 0 :max 1 (group (literal 'a')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse two group zero-or-one quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)?(b)?";
    const expected: []const u8 = "(concat (quantifier :min 0 :max 1 (group (literal 'a'))) (quantifier :min 0 :max 1 (group (literal 'b'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse one-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a+";
    const expected: []const u8 = "(quantifier :min 1 :max null (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse group one-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)+";
    const expected: []const u8 = "(quantifier :min 1 :max null (group (literal 'a')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse two group one-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)+(b)+";
    const expected: []const u8 = "(concat (quantifier :min 1 :max null (group (literal 'a'))) (quantifier :min 1 :max null (group (literal 'b'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a*";
    const expected: []const u8 = "(quantifier :min 0 :max null (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse group zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)*";
    const expected: []const u8 = "(quantifier :min 0 :max null (group (literal 'a')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse two group zero-or-more quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a)*(b)*";
    const expected: []const u8 = "(concat (quantifier :min 0 :max null (group (literal 'a'))) (quantifier :min 0 :max null (group (literal 'b'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse interval simple" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a{1}";
    const expected: []const u8 = "(quantifier :min 1 :max 1 (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse interval range" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a{1,2}";
    const expected: []const u8 = "(quantifier :min 1 :max 2 (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse interval range empty" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a{1,}";
    const expected: []const u8 = "(quantifier :min 1 :max null (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation simple" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|b";
    const expected: []const u8 = "(alternation (literal 'a') (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation multiple" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|b|c";
    const expected: []const u8 = "(alternation (alternation (literal 'a') (literal 'b')) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation precedence vs concat (left)" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "ab|c";
    const expected: []const u8 = "(alternation (concat (literal 'a') (literal 'b')) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation precedence vs concat (right)" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|bc";
    const expected: []const u8 = "(alternation (literal 'a') (concat (literal 'b') (literal 'c')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation precedence vs quantifier (left)" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a*|b";
    const expected: []const u8 = "(alternation (quantifier :min 0 :max null (literal 'a')) (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation precedence vs quantifier (right)" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|b*";
    const expected: []const u8 = "(alternation (literal 'a') (quantifier :min 0 :max null (literal 'b')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation precedence multiple quantifiers" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a?|b+";
    const expected: []const u8 = "(alternation (quantifier :min 0 :max 1 (literal 'a')) (quantifier :min 1 :max null (literal 'b')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation inside group concatenated left" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a|b)c";
    const expected: []const u8 = "(concat (group (alternation (literal 'a') (literal 'b'))) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation inside group concatenated right" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a(b|c)";
    const expected: []const u8 = "(concat (literal 'a') (group (alternation (literal 'b') (literal 'c'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation with groups explicitly left assoc" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a|b)|c";
    const expected: []const u8 = "(alternation (group (alternation (literal 'a') (literal 'b'))) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation with groups explicitly right assoc" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|(b|c)";
    const expected: []const u8 = "(alternation (literal 'a') (group (alternation (literal 'b') (literal 'c'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation complex mix" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a*b|c?d|e";
    const expected: []const u8 = "(alternation (alternation (concat (quantifier :min 0 :max null (literal 'a')) (literal 'b')) (concat (quantifier :min 0 :max 1 (literal 'c')) (literal 'd'))) (literal 'e'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse dot class literal " {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = ".a";
    const expected: []const u8 = "(concat (class :negated #f :count 255) (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse literal class dot " {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a.";
    const expected: []const u8 = "(concat (literal 'a') (class :negated #f :count 255))";
    try expectAstSformExact(allocator, pattern, expected);
}

// test "parse class simple literals" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[abc]";

//     const expected: []const u8 = "(class :negated #f :count 3)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class negated simple literals" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[^abc]";

//     const expected: []const u8 = "(class :negated #t :count 253)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class simple range" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[a-c]";

//     const expected: []const u8 = "(class :negated #f :count 3)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class multiple ranges" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[a-zA-Z]";

//     const expected: []const u8 = "(class :negated #f :count 52)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class ranges and literals" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[a-fA-F0-9]";

//     const expected: []const u8 = "(class :negated #f :count 22)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class literal hyphen at start" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[-ab]";

//     const expected: []const u8 = "(class :negated #f :count 3)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class literal hyphen at end" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[ab-]";

//     const expected: []const u8 = "(class :negated #f :count 3)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class literal closing bracket first" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[]a]";

//     const expected: []const u8 = "(class :negated #f :count 2)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class literal caret not first" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[a^]";

//     const expected: []const u8 = "(class :negated #f :count 2)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class escaped special chars" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[\\-\\]\\^\\\\]";

//     const expected: []const u8 = "(class :negated #f :count 4)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class empty" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[]";

//     const expected: []const u8 = "(class :negated #f :count 0)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class negated empty" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[^]";

//     const expected: []const u8 = "(class :negated #t :count 256)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class dot is literal" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[.a]";

//     const expected: []const u8 = "(class :negated #f :count 2)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class mixed range literal no range" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[a-c-e]";

//     const expected: []const u8 = "(class :negated #f :count 5)";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class concatenated" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "a[bc]";

//     const expected: []const u8 = "(concat (literal 'a') (class :negated #f :count 2))";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class quantified" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "[0-9]+";

//     const expected: []const u8 = "(quantifier :min 1 :max null (class :negated #f :count 10))";
//     try expectAstSformExact(allocator, pattern, expected);
// }

// test "parse class alternated" {
//     const allocator = std.testing.allocator;
//     const pattern: []const u8 = "a|[0-9]";

//     const expected: []const u8 = "(alternation (literal 'a') (class :negated #f :count 10))";
//     try expectAstSformExact(allocator, pattern, expected);
// }
