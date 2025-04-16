const std = @import("std");
const mem = std.mem;
const math = std.math;
const Order = math.Order;
const assert = std.debug.assert;
const Allocator = mem.Allocator;
const Range = std.bit_set.Range;

const Ast = @import("Ast.zig").Ast;
const Node = Ast.Node;
const Lexer = @import("Lexer.zig").Lexer;
const Token = @import("Token.zig").Token;

const Charset = std.bit_set.StaticBitSet(256);
const SubstitutionMap = std.StringArrayHashMap(*Node);

pub const Parser = struct {
    lexer: Lexer,
    token: Token,
    ast: Ast,
    start_with: bool = false,
    end_with: bool = false,
    trailing_ctx: bool = false,
    substitutions: ?*SubstitutionMap,

    pub fn init(gpa: Allocator, pattern: []const u8) Parser {
        var lexer: Lexer = .init(pattern);
        const end = lexer.eatAnchorEnd();
        const start = lexer.eatAnchorStart();
        const initial_token = lexer.next();
        return .{
            .lexer = lexer,
            .token = initial_token,
            .ast = Ast.init(gpa),
            .start_with = start,
            .end_with = end,
            .trailing_ctx = false,
            .substitutions = null,
        };
    }

    pub fn initSubsitutions(gpa: Allocator, pattern: []const u8, substitutions: ?*SubstitutionMap) Parser {
        var lexer: Lexer = .init(pattern);
        const initial_token = lexer.next();
        return .{
            .lexer = lexer,
            .token = initial_token,
            .ast = Ast.init(gpa),
            .substitutions = substitutions,
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
        EmptyClass,
        SyntaxError,
        MissingDefinition,
    } || Allocator.Error;

    fn eats(self: *Parser, n: usize) void {
        assert(n > 0);
        for (0..n) |_| {
            _ = self.next();
        }
    }

    fn next(self: *Parser) Token {
        const prev_token = self.token;
        self.token = self.lexer.next();
        return prev_token;
    }

    fn nextLiteral(self: *Parser) u8 {
        const prev_token = self.token.toU8();
        self.token = self.lexer.next();
        return prev_token;
    }

    fn peek(self: *Parser) Token {
        return self.lexer.peek();
    }

    fn peekLiteral(self: *Parser) u8 {
        return self.lexer.peek().toU8();
    }

    fn curr(self: *Parser) Token {
        return self.token;
    }

    fn currLiteral(self: *Parser) u8 {
        return self.token.toU8();
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
            if (self.start_with and self.end_with) {
                return error.SyntaxError;
            }
            if (self.start_with) {
                const expr = try self.parseExpression(.none);
                self.ast.root = try self.createNode(.{ .start_with = expr });
            } else if (self.end_with) {
                const expr = try self.parseExpression(.none);
                self.ast.root = try self.createNode(.{ .ends_with = expr });
            } else {
                self.ast.root = try self.parseExpression(.none);
            }
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
            .backslash => self.nudEscaped(),
            .left_bracket => self.nudClass(),
            .left_brace => self.nudDefinition(),
            .double_quote => self.nudQuote(),
            .dot => self.nudDot(),
            .eof => error.UnexpectedEOF,
            else => error.SyntaxError,
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
            .backslash => self.ledImplicitConcat(left),
            .asterisk => self.ledZeroOrMore(left),
            .slash => self.ledTrailingContext(left),
            .left_brace => result: {
                if (isIdentifierStart(self.peekLiteral())) {
                    break :result self.ledImplicitConcat(left);
                } else {
                    break :result self.ledIntervalExpression(left);
                }
            },
            .alternation => self.ledAlternation(left),
            .double_quote => try self.ledImplicitConcat(left),
            .eof => error.UnexpectedEOF,
            else => error.SyntaxError,
        };
    }

    fn ledTrailingContext(self: *Parser, left: *Node) ParseError!*Node {
        assert(self.token.kind() == .slash);
        self.eats(1);
        if (self.trailing_ctx == true) {
            return error.SyntaxError;
        } else {
            self.trailing_ctx = true;
        }
        return self.createNode(.{
            .trailing = .{
                .lhs = left,
                .rhs = try self.parseExpression(.trailing),
            },
        });
    }

    fn isIdentifierStart(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_';
    }

    fn isIdentifierInner(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    fn nudDefinition(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .left_brace);
        self.eats(1);

        if (!isIdentifierStart(self.currLiteral())) {
            return error.SyntaxError;
        }

        var buffer = std.BoundedArray(u8, 256).init(0) catch unreachable;
        while (true) {
            if (self.currLiteral() == 0x00) {
                return error.UnclosedIdentifier;
            }

            if (self.currLiteral() == '}') {
                break;
            }

            if (!isIdentifierInner(self.currLiteral())) {
                return error.SyntaxError;
            }

            buffer.appendAssumeCapacity(self.nextLiteral());
        }
        assert(self.token.kind() == .right_brace);
        self.eats(1);

        const key = buffer.constSlice();
        if (self.substitutions) |sub| {
            const value = sub.get(key) orelse return error.MissingDefinition;
            return self.createNode(.{
                .group = try self.ast.cloneNode(value),
            });
        } else {
            return error.MissingDefinition;
        }
    }

    fn nudQuote(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .double_quote);
        self.eats(1);

        var buffer = std.BoundedArray(u8, 256).init(0) catch unreachable;
        while (true) {
            if (self.currLiteral() == '"') {
                break;
            }

            if (self.currLiteral() == 0x00) {
                return error.UnclosedQuote;
            }
            buffer.appendAssumeCapacity(self.nextLiteral());
        }
        assert(self.token.kind() == .double_quote);
        self.eats(1);
        const allocator = self.ast.scratchAllocator();
        const string = try allocator.dupe(u8, buffer.constSlice());
        return self.createNode(.{
            .quoted = .{
                .string = string,
            },
        });
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
        if (self.token.kind() == .eof) {
            return error.UnclosedParenthesis;
        }

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

    fn parsePosixClass(self: *Parser) ParseError!Charset {
        assert(self.token.kind() == .left_bracket);
        self.eats(1);
        assert(self.token.kind() == .colon);
        self.eats(1);

        var buffer = std.BoundedArray(u8, 256).init(0) catch unreachable;
        while (true) {
            if (self.currLiteral() == ':' and self.peekLiteral() == ']') {
                self.eats(2);
                break;
            }
            buffer.appendAssumeCapacity(self.currLiteral());
            self.eats(1);
        }
        const class = std.meta.stringToEnum(PosixClass, buffer.constSlice()) orelse return error.SyntaxError;
        return class.toCharset();
    }

    fn parseHexaSequence(self: *Parser) ParseError!Token {
        assert((self.currLiteral() == 'x' or self.currLiteral() == 'X'));
        self.eats(1);

        var buffer = std.BoundedArray(u8, 3).init(0) catch unreachable;
        for (0..2) |_| {
            const c = self.currLiteral();
            if (std.ascii.isHex(c)) {
                buffer.appendAssumeCapacity(self.nextLiteral());
            } else {
                break;
            }
        }
        const value: u8 = std.fmt.parseUnsigned(u8, buffer.constSlice(), 16) catch {
            return error.SyntaxError;
        };
        return Token.init(.literal, value);
    }

    fn parseOctalSequence(self: *Parser, len: usize) ParseError!Token {
        assert((self.currLiteral() >= '0' and self.currLiteral() <= '7') or (self.currLiteral() == 'o'));

        if (len == 2) {
            self.eats(1);
        }

        var buffer = std.BoundedArray(u8, 3).init(0) catch unreachable;
        for (0..len) |_| {
            const c = self.currLiteral();
            if (c >= '0' and c <= '7') {
                buffer.appendAssumeCapacity(self.nextLiteral());
            } else {
                break;
            }
        }

        const value: u8 = std.fmt.parseUnsigned(u8, buffer.constSlice(), 8) catch {
            return error.SyntaxError;
        };

        return Token.init(.literal, value);
    }

    fn parseEscapedSequence(self: *Parser) ParseError!Token {
        assert(self.token.kind() == .backslash);
        self.eats(1);
        const token = self.currLiteral();
        return switch (token) {
            0x00 => error.UnexpectedEOF,
            'n' => tok: {
                self.eats(1);
                const esc_token = Token.init(.literal, '\n');
                break :tok esc_token;
            },
            't' => tok: {
                const esc_token = Token.init(.literal, '\t');
                self.eats(1);
                break :tok esc_token;
            },
            'r' => tok: {
                const esc_token = Token.init(.literal, '\r');
                self.eats(1);
                break :tok esc_token;
            },
            'f' => tok: {
                const esc_token = Token.init(.literal, std.ascii.control_code.ff);
                self.eats(1);
                break :tok esc_token;
            },
            'v' => tok: {
                const esc_token = Token.init(.literal, std.ascii.control_code.vt);
                self.eats(1);
                break :tok esc_token;
            },
            '0'...'7' => try self.parseOctalSequence(3),
            'o' => try self.parseOctalSequence(2),
            'x', 'X' => try self.parseHexaSequence(),
            else => tok: {
                const lit_token = Token.init(.literal, token);
                self.eats(1);
                break :tok lit_token;
            },
        };
    }

    fn nudEscaped(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .backslash);
        const token = try self.parseEscapedSequence();
        return self.createNode(.{
            .literal = token.literal,
        });
    }

    fn nudClass(self: *Parser) ParseError!*Node {
        assert(self.token.kind() == .left_bracket);
        self.eats(1);

        var negated: bool = false;
        var charset: Charset = .initEmpty();

        if (self.currLiteral() == '^') {
            negated = true;
            self.eats(1);
        }

        if (negated and self.currLiteral() == ']' and self.peekLiteral() == 0x00) {
            self.eats(1);
            charset.set('^');
            return self.createNode(.{ .class = .{
                .negated = false,
                .charset = charset,
            } });
        }

        if (self.currLiteral() == ']' or self.currLiteral() == '-' and self.peekLiteral() != 0x00) {
            charset.set(self.nextLiteral());
        }

        if (self.currLiteral() == 0x00) {
            return error.EmptyClass;
        }

        while (true) {
            if (self.currLiteral() == 0x00) {
                return error.UnclosedBracket;
            }

            if (self.currLiteral() == ']') {
                break;
            }

            if (self.currLiteral() == '[' and self.peekLiteral() == ':') {
                const class = try self.parsePosixClass();
                charset.setUnion(class);
                continue;
            }

            if (self.token.kind() == .backslash) {
                const escaped = try self.parseEscapedSequence();
                charset.set(escaped.toU8());
                continue;
            }

            if (self.peekLiteral() == '-') {
                var range: Range = .{
                    .start = self.currLiteral(),
                    .end = 0,
                };
                self.eats(2);
                if (self.currLiteral() == ']') {
                    charset.set(range.start);
                    charset.set('-');
                    continue;
                }

                range.end = self.currLiteral();
                if (range.end < range.start) {
                    charset.set(range.start);
                    charset.set('-');
                    charset.set(range.end);
                } else {
                    charset.setRangeValue(range, true);
                }
                continue;
            }
            charset.set(self.nextLiteral());
        }
        assert(self.currLiteral() == ']');
        self.eats(1);
        return self.createNode(.{
            .class = .{
                .negated = negated,
                .charset = charset,
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
            .backslash => @intFromEnum(BindingPower.escaped),
            .double_quote => @intFromEnum(BindingPower.quoting),
            .slash => @intFromEnum(BindingPower.trailing),
            .eof => @intFromEnum(BindingPower.none),
            .dot => @intFromEnum(BindingPower.class),
            else => @intFromEnum(BindingPower.none),
        };
        const min_bp_value: u8 = @intFromEnum(bp);
        return math.order(current_bp_value, min_bp_value);
    }

    pub const BindingPower = enum(u8) {
        none = 0,
        trailing = 1,
        alternation = 2,
        interval_expression = 3,
        concatenation = 4,
        quantifier = 5,
        definition = 6,
        grouping = 7,
        quoting = 8,
        class = 9,
        escaped = 10,
        bracket = 11,
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

fn expectAstDefinitionSformExact(gpa: Allocator, substitutions: *SubstitutionMap, pattern: []const u8, expected: []const u8) !void {
    var parser: Parser = .initSubsitutions(gpa, pattern, substitutions);
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

test "parse class simple literals alpha" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[[:alpha:]]";

    const expected: []const u8 = "(class :negated #f :count 52)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class simple literals alnum" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[[:alnum:]]";

    const expected: []const u8 = "(class :negated #f :count 62)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class simple literals abc" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[abc]";

    const expected: []const u8 = "(class :negated #f :count 3)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class simple range lowercase" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[a-z]";

    const expected: []const u8 = "(class :negated #f :count 26)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class simple range lowercase, implicit concat" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[a-z]a";

    const expected: []const u8 = "(concat (class :negated #f :count 26) (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class negated literals" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[^123]";

    const expected: []const u8 = "(class :negated #t :count 3)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse class literal hyphen at end" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[a-c-]";

    const expected: []const u8 = "(class :negated #f :count 4)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped tab" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\t";
    const expected: []const u8 = "(literal 'tab')";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped quantifier metacharacter" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\?";
    const expected: []const u8 = "(literal '?')";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse hex escape in concatenation" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a\\x42c";
    const expected: []const u8 = "(concat (concat (literal 'a') (literal 'B')) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse octal escape quantified" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\60+";
    const expected: []const u8 = "(quantifier :min 1 :max null (literal '0'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped backslash in concatenation" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a\\\\b";

    const expected: []const u8 = "(concat (concat (literal 'a') (literal 'escaped')) (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escapes within quantified alternation group" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "(a\\?|b\\*)c";
    const expected: []const u8 = "(concat (group (alternation (concat (literal 'a') (literal '?')) (concat (literal 'b') (literal '*')))) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped character with interval quantifier" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\x2A{2,3}";
    const expected: []const u8 = "(quantifier :min 2 :max 3 (literal '*'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse max octal escape followed by literal digit" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\1014";
    const expected: []const u8 = "(concat (literal 'A') (literal '4'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped brackets and braces" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\[a\\{b\\}\\]";

    const expected: []const u8 = "(concat (concat (literal '[') (concat (literal 'a') (literal '{'))) (concat (literal 'b') (concat (literal '}') (literal ']'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse multiple mixed escapes concatenated" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\t\\x41\\\\\\(\\n";

    const expected: []const u8 = "(concat (literal 'tab') (concat (literal 'A') (concat (literal 'escaped') (concat (literal '(') (literal 'newline')))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse quoted simple string" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\"abc\"";
    const expected: []const u8 = "(quoted abc)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse quoted string with metacharacters" {
    const allocator = std.testing.allocator;

    const pattern: []const u8 = "\"a*b.\"";
    const expected: []const u8 = "(quoted a*b.)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse quoted empty string" {
    const allocator = std.testing.allocator;

    const pattern: []const u8 = "\"\"";
    const expected: []const u8 = "(quoted )";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse concatenation with quoted string" {
    const allocator = std.testing.allocator;

    const pattern: []const u8 = "a\"b*c\"d";
    const expected: []const u8 = "(concat (concat (literal 'a') (quoted b*c)) (literal 'd'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse definition simple" {
    const allocator = std.testing.allocator;

    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var parser = Parser.init(allocator, "[0-9]");
    defer parser.deinit();

    const digit_definiton = try parser.parse();
    if (digit_definiton.root) |node| {
        try substitutions.put("DIGIT", node);
    }

    const pattern: []const u8 = "{DIGIT}+";
    const expected: []const u8 = "(quantifier :min 1 :max null (group (class :negated #f :count 10)))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);
}

test "parse definition complex" {
    const allocator = std.testing.allocator;

    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var parser = Parser.init(allocator, "[0-9]");
    defer parser.deinit();

    const digit_definiton = try parser.parse();
    if (digit_definiton.root) |node| {
        try substitutions.put("DIGIT", node);
    }

    const pattern: []const u8 = "{DIGIT}{1,5}";
    const expected: []const u8 = "(quantifier :min 1 :max 5 (group (class :negated #f :count 10)))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);
}

test "parse definition complex twice" {
    const allocator = std.testing.allocator;

    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var parser = Parser.init(allocator, "[0-9]");
    defer parser.deinit();

    const digit_definiton = try parser.parse();
    if (digit_definiton.root) |node| {
        try substitutions.put("DIGIT", node);
    }

    const pattern: []const u8 = "{DIGIT}{1,5}{DIGIT}?";
    const expected: []const u8 = "(concat (quantifier :min 1 :max 5 (group (class :negated #f :count 10))) (quantifier :min 0 :max 1 (group (class :negated #f :count 10))))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);
}

test "parse anchor start" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "^a", "(start_with (literal 'a'))");
}

test "parse anchor end" {
    const allocator = std.testing.allocator;
    try expectAstSformExact(allocator, "a$", "(ends_with (literal 'a'))");
}

const testing = std.testing;

test "parse trailing context simple" {
    const allocator = testing.allocator;
    const pattern = "a/b";

    const expected = "(trailing (literal 'a') (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context complex main expression" {
    const allocator = testing.allocator;
    const pattern = "a+b?/(cd)*";

    const expected = "(trailing (concat (quantifier :min 1 :max null (literal 'a')) (quantifier :min 0 :max 1 (literal 'b'))) (quantifier :min 0 :max null (group (concat (literal 'c') (literal 'd')))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context complex context expression" {
    const allocator = testing.allocator;
    const pattern = "[^0-9]/(\\t|\\n)+";

    const expected = "(trailing (class :negated #t :count 10) (quantifier :min 1 :max null (group (alternation (literal 'tab') (literal 'newline')))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context precedence vs alternation (left)" {
    const allocator = testing.allocator;
    const pattern = "a|b/c";

    const expected = "(trailing (alternation (literal 'a') (literal 'b')) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context precedence vs alternation (right)" {
    const allocator = testing.allocator;
    const pattern = "a/b|c";

    const expected = "(trailing (literal 'a') (alternation (literal 'b') (literal 'c')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context precedence vs alternation (both)" {
    const allocator = testing.allocator;
    const pattern = "a|b/c|d";

    const expected = "(trailing (alternation (literal 'a') (literal 'b')) (alternation (literal 'c') (literal 'd')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context with start anchor" {
    const allocator = testing.allocator;
    const pattern = "^a+/b";

    const expected = "(start_with (trailing (quantifier :min 1 :max null (literal 'a')) (literal 'b')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context with end anchor" {
    const allocator = testing.allocator;
    const pattern = "a/b+$";

    const expected = "(ends_with (trailing (literal 'a') (quantifier :min 1 :max null (literal 'b'))))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escaped slash (no trailing context)" {
    const allocator = testing.allocator;
    const pattern = "a\\/b";
    const expected = "(concat (concat (literal 'a') (literal '/')) (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse slash inside character class" {
    const allocator = testing.allocator;
    const pattern = "[a/z]";

    const expected = "(class :negated #f :count 3)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse slash inside quoted string" {
    const allocator = testing.allocator;
    const pattern = "\"a/b\"";
    const expected = "(quoted a/b)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class empty" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[]");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.EmptyClass, parser.parse());
}

test "parse multiple trailing contexts error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a/b/c");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse error unclosed parenthesis open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "(a");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnclosedParenthesis, parser.parse());
}

test "parse character class empty negated" {
    const allocator = std.testing.allocator;

    const pattern: []const u8 = "[^]]";
    const expected: []const u8 = "(class :negated #t :count 1)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class range with escaped hex" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[\\x41-\\x43]";
    const expected: []const u8 = "(class :negated #f :count 3)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class hyphen at beginning" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[-abc]";
    const expected: []const u8 = "(class :negated #f :count 4)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class caret not at beginning" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[a^bc]";
    const expected: []const u8 = "(class :negated #f :count 4)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class complex mix" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[a-f[:digit:]_\\t-]";
    const expected: []const u8 = "(class :negated #f :count 19)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class negated posix" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[^[:digit:]]";
    const expected: []const u8 = "(class :negated #t :count 10)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class escaped special chars" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[\\^\\-\\]\\\\]";
    const expected: []const u8 = "(class :negated #f :count 4)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse escape at end of pattern" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a\\");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnexpectedEOF, parser.parse());
}

test "parse escapes inside quoted string" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\"a\\nb\\tc\"";
    const expected: []const u8 = "(quoted a\\nb\\tc)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse interval quantifier zero min" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a{0,5}";
    const expected: []const u8 = "(quantifier :min 0 :max 5 (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse interval quantifier invalid syntax comma start" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{,5}");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse interval quantifier invalid syntax non digit" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1a}");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse interval quantifier invalid range max less than min" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a{5,2}";
    const expected: []const u8 = "(quantifier :min 5 :max 2 (literal 'a'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse definition undefined" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();
    var parser = Parser.initSubsitutions(allocator, "{UNDEFINED}", &substitutions);
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.MissingDefinition, parser.parse());
}

test "parse definition empty identifier" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();
    var parser = Parser.initSubsitutions(allocator, "{}", &substitutions);
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse definition invalid identifier start" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();
    var parser = Parser.initSubsitutions(allocator, "{1A}", &substitutions);
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse definition invalid identifier inner" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();
    var parser = Parser.initSubsitutions(allocator, "{A!}", &substitutions);
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse definition unclosed identifier" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();
    var parser = Parser.initSubsitutions(allocator, "{A", &substitutions);
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnclosedIdentifier, parser.parse());
}

test "parse both anchors error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "^a$");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse anchor start mid pattern" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a^b";
    const expected: []const u8 = "(concat (concat (literal 'a') (literal '^')) (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse anchor end mid pattern" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a$b";
    const expected: []const u8 = "(concat (concat (literal 'a') (literal '$')) (literal 'b'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context no context" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a/");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnexpectedEOF, parser.parse());
}

test "parse trailing context no main expression" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "/b");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse error unclosed parenthesis close" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a)");
    defer parser.deinit();

    const pattern: []const u8 = "a)";
    const expected: []const u8 = "(literal 'a')";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse error unclosed bracket open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[a");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnclosedBracket, parser.parse());
}

test "parse error unclosed brace interval open" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse error unclosed brace interval comma" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1,");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse error unclosed quote" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "\"abc");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.UnclosedQuote, parser.parse());
}

test "parse error empty group" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "()");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.EmptyGroup, parser.parse());
}

test "parse error invalid posix class" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[[:invalid:]]");
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse complex alternation interaction" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a[bc]*|\\d{2,}/e";
    const expected: []const u8 = "(alternation (concat (literal 'a') (quantifier :min 0 :max null (class :negated #f :count 2))) (quantifier :min 2 :max null (literal 'd')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse quantifier on quoted string error" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\"abc\"*";

    const expected: []const u8 = "(quantifier :min 0 :max null (quoted abc))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse alternation with quoted string" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a|\"b*\"|c";
    const expected: []const u8 = "(alternation (alternation (literal 'a') (quoted b*)) (literal 'c'))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse definition nested" {
    const allocator = std.testing.allocator;

    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var inner_parser = Parser.init(allocator, "\"b\"");
    defer inner_parser.deinit();
    const inner_ast = try inner_parser.parse();
    if (inner_ast.root) |node| {
        try substitutions.put("INNER", node);
    } else return error.TestUnexpectedParsingFailure;

    var outer_parser = Parser.initSubsitutions(allocator, "a{INNER}c", &substitutions);
    defer outer_parser.deinit();
    const outer_ast = try outer_parser.parse();
    if (outer_ast.root) |node| {
        try substitutions.put("OUTER", node);
    } else return error.TestUnexpectedParsingFailure;

    const pattern: []const u8 = "{OUTER}+";

    const expected: []const u8 = "(quantifier :min 1 :max null (group (concat (concat (literal 'a') (group (quoted b))) (literal 'c'))))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);
}

test "parse character class caret only" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[^]";
    const expected: []const u8 = "(class :negated #f :count 1)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class negated hyphen" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "[^-]";
    const expected: []const u8 = "(class :negated #t :count 1)";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse character class posix inside range invalid" {
    const allocator = std.testing.allocator;

    const expected: []const u8 = "(class :negated #f :count 13)";
    try expectAstSformExact(allocator, "[a-[:digit:]]", expected);
}

test "parse adjacent quantifiers error" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a??";
    const expected: []const u8 = "(quantifier :min 0 :max 1 (quantifier :min 0 :max 1 (literal 'a')))";
    try expectAstSformExact(allocator, pattern, expected);

    const pattern2: []const u8 = "a+*";
    const expected2: []const u8 = "(quantifier :min 0 :max null (quantifier :min 1 :max null (literal 'a')))";
    try expectAstSformExact(allocator, pattern2, expected2);
}

test "parse quantifier on start anchor error" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, "^?");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse quantifier on end anchor error" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a$?";
    const expected: []const u8 = "(concat (literal 'a') (quantifier :min 0 :max 1 (literal '$')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse empty alternative start error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "|b");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse empty alternative end error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a|");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.UnexpectedEOF, parser.parse());
}

test "parse empty alternative middle error" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a||b");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}

test "parse alternation with anchors" {
    const allocator = std.testing.allocator;

    const pattern: []const u8 = "^a|b$";

    var parser = Parser.init(allocator, pattern);
    defer parser.deinit();
    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());

    const pattern_start: []const u8 = "^a|b";
    const expected_start: []const u8 = "(start_with (alternation (literal 'a') (literal 'b')))";
    try expectAstSformExact(allocator, pattern_start, expected_start);

    const pattern_end: []const u8 = "a|b$";
    const expected_end: []const u8 = "(ends_with (alternation (literal 'a') (literal 'b')))";
    try expectAstSformExact(allocator, pattern_end, expected_end);
}

test "parse trailing context complex quantifiers" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "a?/b*";

    const expected: []const u8 = "(trailing (quantifier :min 0 :max 1 (literal 'a')) (quantifier :min 0 :max null (literal 'b')))";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse trailing context with definition" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var parser = Parser.init(allocator, "[0-9]");
    defer parser.deinit();
    const digit_def = try parser.parse();
    if (digit_def.root) |node| {
        try substitutions.put("DIGIT", node);
    } else return error.TestUnexpectedParsingFailure;

    const pattern: []const u8 = "a/{DIGIT}+";

    const expected: []const u8 = "(trailing (literal 'a') (quantifier :min 1 :max null (group (class :negated #f :count 10))))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);
}

test "parse definition yielding complex structure" {
    const allocator = std.testing.allocator;
    var substitutions = SubstitutionMap.init(allocator);
    defer substitutions.deinit();

    var parser = Parser.init(allocator, "a|b");
    defer parser.deinit();
    const alt_def = try parser.parse();
    if (alt_def.root) |node| {
        try substitutions.put("ALT", node);
    } else return error.TestUnexpectedParsingFailure;

    const pattern: []const u8 = "{ALT}*";
    const expected: []const u8 = "(quantifier :min 0 :max null (group (alternation (literal 'a') (literal 'b'))))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern, expected);

    const pattern2: []const u8 = "c{ALT}d";
    const expected2: []const u8 = "(concat (concat (literal 'c') (group (alternation (literal 'a') (literal 'b')))) (literal 'd'))";
    try expectAstDefinitionSformExact(allocator, &substitutions, pattern2, expected2);
}

test "parse anchor empty pattern start" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "^";
    const expected: []const u8 = "null";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse anchor empty pattern end" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "$";
    const expected: []const u8 = "null";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse error mismatched delimiters" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "([a-z)}");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.UnclosedBracket, parser.parse());
}

test "parse unknown escape sequence" {
    const allocator = std.testing.allocator;
    const pattern: []const u8 = "\\z";
    const expected: []const u8 = "(literal 'z')";
    try expectAstSformExact(allocator, pattern, expected);
}

test "parse error syntax in interval quantifier" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a{1,b}");
    defer parser.deinit();

    try std.testing.expectError(Parser.ParseError.SyntaxError, parser.parse());
}
