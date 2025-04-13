const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const debug = std.debug;
const assert = debug.assert;
const ascii = std.ascii;

pub const Lexer = struct {
    inputs: []const u8,
    index: usize,
    mode: Mode,
    is_first_in_bracket: bool,

    pub const Mode = enum {
        in_regex_expr,
        in_posix_expr,
        in_colla_expr,
        in_equiv_expr,
        in_class_expr,
        in_intrv_expr,
    };

    pub fn init(inputs: []const u8) Lexer {
        return .{
            .inputs = inputs,
            .index = 0,
            .mode = .in_regex_expr,
            .is_first_in_bracket = false,
        };
    }

    fn advance(self: *Lexer, count: usize) void {
        self.index += count;
    }

    pub fn next(self: *Lexer) Token {
        const res = self.peek();
        self.advance(res.bytes);
        return res.token;
    }

    pub fn peek(self: *Lexer) TokenResult {
        const token_res = self.tokenize();

        switch (token_res.token) {
            .open_bracket => self.mode = .in_posix_expr,
            .open_interval_brace => self.mode = .in_intrv_expr,
            .close_bracket => self.mode = .in_regex_expr,
            .close_interval_brace => self.mode = .in_regex_expr,
            .character_class_start => self.mode = .in_class_expr,
            .character_class_end => self.mode = .in_posix_expr,
            .collating_element_start => self.mode = .in_colla_expr,
            .collating_element_end => self.mode = .in_posix_expr,
            .equivalence_class_start => self.mode = .in_equiv_expr,
            .equivalence_class_end => self.mode = .in_posix_expr,
            else => {
                if (self.mode == .in_posix_expr and self.is_first_in_bracket) {
                    self.is_first_in_bracket = false;
                }
            },
        }

        if (token_res.token == .open_bracket) {
            self.is_first_in_bracket = true;
        }

        return token_res;
    }

    fn tokenize(self: *const Lexer) TokenResult {
        if (self.index >= self.inputs.len) {
            return TokenResult.EOF;
        }

        return switch (self.mode) {
            .in_regex_expr => self.tokenizeRegexExpr(),
            .in_posix_expr => self.tokenizePosixExpr(),
            .in_colla_expr => self.tokenizeCollaExpr(),
            .in_equiv_expr => self.tokenizeEquivExpr(),
            .in_class_expr => self.tokenizeClassExpr(),
            .in_intrv_expr => self.tokenizeIntrvExpr(),
        };
    }

    fn tokenizeRegexExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_regex_expr);
        const c = self.inputs[self.index];

        switch (c) {
            '\\' => {
                if (self.index + 1 >= self.inputs.len) {
                    return .{ .token = .{ .literal = '\\' }, .bytes = 1 };
                }
                const next_c = self.inputs[self.index + 1];

                if (ascii.isDigit(next_c)) {
                    const digit = next_c - '0';
                    if (digit >= 1 and digit <= 9) {
                        return .{ .token = .{ .backreference = @intCast(digit) }, .bytes = 2 };
                    }
                }

                switch (next_c) {
                    '^', '$', '.', '[', '(', ')', '|', '*', '+', '?', '{', '\\' => {
                        return .{ .token = .{ .literal = next_c }, .bytes = 2 };
                    },
                    else => {
                        return .{ .token = .{ .literal = next_c }, .bytes = 2 };
                    },
                }
            },

            '^' => return .{ .token = .start_anchor, .bytes = 1 },
            '$' => return .{ .token = .end_anchor, .bytes = 1 },
            '.' => return .{ .token = .any_character, .bytes = 1 },
            '|' => return .{ .token = .alternation, .bytes = 1 },
            '*' => return .{ .token = .zero_or_more, .bytes = 1 },
            '+' => return .{ .token = .one_or_more, .bytes = 1 },
            '?' => return .{ .token = .zero_or_one, .bytes = 1 },
            '(' => return .{ .token = .open_group, .bytes = 1 },
            ')' => return .{ .token = .close_group, .bytes = 1 },
            '[' => return .{ .token = .open_bracket, .bytes = 1 },
            '{' => return .{ .token = .open_interval_brace, .bytes = 1 },

            else => return .{ .token = .{ .literal = c }, .bytes = 1 },
        }
    }

    fn tokenizeClassExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_class_expr);

        if (self.index + 1 < self.inputs.len and
            self.inputs[self.index] == ':' and
            self.inputs[self.index + 1] == ']')
        {
            return .{ .token = .character_class_end, .bytes = 2 };
        }

        var end = self.index;
        while (end + 1 < self.inputs.len) {
            if (self.inputs[end] == ':' and self.inputs[end + 1] == ']') {
                break;
            }

            end += 1;
        }

        if (end + 1 >= self.inputs.len or self.inputs[end] != ':' or self.inputs[end + 1] != ']') {
            std.debug.print("Error: Unterminated character class name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }

        if (end > self.index) {
            const name_slice = self.inputs[self.index..end];

            return .{ .token = .{ .character_class_name = name_slice }, .bytes = end - self.index };
        } else {
            std.debug.print("Error: Empty character class name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }
    }

    fn tokenizeCollaExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_colla_expr);

        if (self.index + 1 < self.inputs.len and
            self.inputs[self.index] == '.' and
            self.inputs[self.index + 1] == ']')
        {
            return .{ .token = .collating_element_end, .bytes = 2 };
        }

        var end = self.index;
        while (end + 1 < self.inputs.len) {
            if (self.inputs[end] == '.' and self.inputs[end + 1] == ']') {
                break;
            }

            end += 1;
        }

        if (end + 1 >= self.inputs.len or self.inputs[end] != '.' or self.inputs[end + 1] != ']') {
            std.debug.print("Error: Unterminated collating element name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }

        if (end > self.index) {
            const name_slice = self.inputs[self.index..end];

            return .{ .token = .{ .collating_element_name = name_slice }, .bytes = end - self.index };
        } else {
            std.debug.print("Error: Empty collating element name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }
    }

    fn tokenizeEquivExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_equiv_expr);

        if (self.index + 1 < self.inputs.len and
            self.inputs[self.index] == '=' and
            self.inputs[self.index + 1] == ']')
        {
            return .{ .token = .equivalence_class_end, .bytes = 2 };
        }

        var end = self.index;
        while (end + 1 < self.inputs.len) {
            if (self.inputs[end] == '=' and self.inputs[end + 1] == ']') {
                break;
            }

            end += 1;
        }

        if (end + 1 >= self.inputs.len or self.inputs[end] != '=' or self.inputs[end + 1] != ']') {
            std.debug.print("Error: Unterminated equivalence class name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }

        if (end > self.index) {
            const name_slice = self.inputs[self.index..end];

            return .{ .token = .{ .equivalence_class_name = name_slice }, .bytes = end - self.index };
        } else {
            std.debug.print("Error: Empty equivalence class name at index {d}\n", .{self.index});
            return TokenResult.EOF;
        }
    }
    fn tokenizePosixExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_posix_expr);
        const c = self.inputs[self.index];

        switch (c) {
            ']' => {
                if (self.is_first_in_bracket) {
                    return .{ .token = .{ .literal = c }, .bytes = 1 };
                } else {
                    return .{ .token = .close_bracket, .bytes = 1 };
                }
            },
            '[' => {
                if (self.index + 1 >= self.inputs.len) {
                    return .{ .token = .{ .literal = c }, .bytes = 1 };
                }
                const next_c = self.inputs[self.index + 1];
                switch (next_c) {
                    '.' => return .{ .token = .collating_element_start, .bytes = 2 },
                    '=' => return .{ .token = .equivalence_class_start, .bytes = 2 },
                    ':' => return .{ .token = .character_class_start, .bytes = 2 },
                    else => return .{ .token = .{ .literal = c }, .bytes = 1 },
                }
            },
            '^' => {
                if (self.is_first_in_bracket) {
                    return .{ .token = .bracket_negation, .bytes = 1 };
                } else {
                    return .{ .token = .{ .literal = c }, .bytes = 1 };
                }
            },
            '-' => {
                if (!self.is_first_in_bracket and self.index + 1 < self.inputs.len and self.inputs[self.index + 1] != ']') {
                    return .{ .token = .bracket_range_operator, .bytes = 1 };
                } else {
                    return .{ .token = .{ .literal = c }, .bytes = 1 };
                }
            },

            '\\' => {
                if (self.index + 1 < self.inputs.len) {
                    return .{ .token = .{ .literal = self.inputs[self.index + 1] }, .bytes = 2 };
                } else {
                    return .{ .token = .{ .literal = c }, .bytes = 1 };
                }
            },
            else => return .{ .token = .{ .literal = c }, .bytes = 1 },
        }
    }

    fn tokenizeIntrvExpr(self: *const Lexer) TokenResult {
        assert(self.mode == .in_intrv_expr);
        const c = self.inputs[self.index];

        switch (c) {
            '}' => return .{ .token = .close_interval_brace, .bytes = 1 },
            ',' => return .{ .token = .interval_comma, .bytes = 1 },
            else => {
                if (ascii.isDigit(c)) {
                    var end = self.index + 1;
                    while (end < self.inputs.len and ascii.isDigit(self.inputs[end])) : (end += 1) {}
                    const num_slice = self.inputs[self.index..end];
                    const num = std.fmt.parseUnsigned(u32, num_slice, 10) catch {
                        return .{ .token = .{ .literal = '{' }, .bytes = 1 };
                    };
                    return .{ .token = .{ .interval_number = num }, .bytes = end - self.index };
                } else {
                    return .{ .token = .{ .literal = '{' }, .bytes = 1 };
                }
            },
        }
    }
};
pub const TokenResult = struct {
    token: Token,
    bytes: usize,

    pub const EOF: TokenResult = .{ .token = .{ .end_of_expression = {} }, .bytes = 0 };
};

pub const Token = union(Kind) {
    literal: u21,
    start_anchor: void,
    end_anchor: void,
    any_character: void,
    alternation: void,
    zero_or_more: void,
    one_or_more: void,
    zero_or_one: void,
    open_group: void,
    close_group: void,
    open_bracket: void,
    close_bracket: void,
    open_interval_brace: void,
    close_interval_brace: void,
    interval_comma: void,
    interval_number: u32,
    backslash: void,
    backreference: u8,
    bracket_range_operator: void,
    bracket_negation: void,
    character_class_start: void,
    character_class_end: void,
    character_class_name: []const u8,
    collating_element_start: void,
    collating_element_end: void,
    collating_element_name: []const u8,
    equivalence_class_start: void,
    equivalence_class_end: void,
    equivalence_class_name: []const u8,
    end_of_expression: void,

    pub const Kind = enum {
        literal,
        start_anchor,
        end_anchor,
        any_character,
        alternation,
        zero_or_more,
        one_or_more,
        zero_or_one,
        open_group,
        close_group,
        open_bracket,
        close_bracket,
        open_interval_brace,
        close_interval_brace,
        interval_comma,
        interval_number,
        backslash,
        backreference,
        bracket_range_operator,
        bracket_negation,
        character_class_start,
        character_class_end,
        character_class_name,
        collating_element_start,
        collating_element_end,
        collating_element_name,
        equivalence_class_start,
        equivalence_class_end,
        equivalence_class_name,
        end_of_expression,
    };

    pub fn tag(self: *const Token) Token.Kind {
        return std.meta.activeTag(self.*);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .start_anchor => try writer.print("^", .{}),
            .end_anchor => try writer.print("$", .{}),
            .any_character => try writer.print(".", .{}),
            .alternation => try writer.print("|", .{}),
            .zero_or_more => try writer.print("*", .{}),
            .one_or_more => try writer.print("+", .{}),
            .zero_or_one => try writer.print("?", .{}),
            .open_group => try writer.print("(", .{}),
            .close_group => try writer.print(")", .{}),
            .open_bracket => try writer.print("[", .{}),
            .close_bracket => try writer.print("]", .{}),
            .open_interval_brace => try writer.print("{{", .{}),
            .close_interval_brace => try writer.print("}}", .{}),
            .interval_comma => try writer.print(",", .{}),
            .backslash => try writer.print("\\", .{}),
            .bracket_range_operator => try writer.print("-", .{}),
            .bracket_negation => try writer.print("^", .{}),
            .character_class_start => try writer.print("[:", .{}),
            .character_class_end => try writer.print(":]", .{}),
            .collating_element_start => try writer.print("[.", .{}),
            .collating_element_end => try writer.print(".]", .{}),
            .equivalence_class_start => try writer.print("[=", .{}),
            .equivalence_class_end => try writer.print("=]", .{}),
            .literal => |v| try writer.print("{u}", .{v}),
            .interval_number => |v| try writer.print("{d}", .{v}),
            .backreference => |v| try writer.print("\\{d}", .{v}),
            .character_class_name => |v| try writer.print("{s}", .{v}),
            .collating_element_name => |v| try writer.print("{s}", .{v}),
            .equivalence_class_name => |v| try writer.print("{s}", .{v}),
            .end_of_expression => try writer.print("EOF", .{}),
        }
    }
};

const testing = std.testing;

fn expectTokens(input: []const u8, expected_kinds: []const Token.Kind) !void {
    var lexer = Lexer.init(input);
    var actual_tokens = std.ArrayList(Token.Kind).init(testing.allocator);
    defer actual_tokens.deinit();

    while (true) {
        const token_res = lexer.peek();
        const token = token_res.token;
        if (token == .end_of_expression) break;
        try actual_tokens.append(token.tag());
        lexer.advance(token_res.bytes);
        if (token_res.bytes == 0 and token != .end_of_expression) {
            std.debug.print("Error: Tokenizer returned 0 bytes for token {any}\n", .{token});
            return error.ZeroByteToken;
        }
        if (lexer.index > input.len) {
            std.debug.print("Error: Lexer index out of bounds\n", .{});
            return error.IndexOutOfBounds;
        }
        std.debug.print("{}", .{token});
    }

    std.debug.print("\n", .{});
    try testing.expectEqualSlices(Token.Kind, expected_kinds, actual_tokens.items);
}

test "lexer literals" {
    const input: []const u8 = "abc";
    const expected = [_]Token.Kind{ .literal, .literal, .literal };
    try expectTokens(input, &expected);
}

test "lexer alternation" {
    const input: []const u8 = "a|b";
    const expected = [_]Token.Kind{ .literal, .alternation, .literal };
    try expectTokens(input, &expected);
}

test "lexer anchors" {
    const input: []const u8 = "^ab$";
    const expected = [_]Token.Kind{ .start_anchor, .literal, .literal, .end_anchor };
    try expectTokens(input, &expected);
}

test "lexer any character" {
    const input: []const u8 = "a.c";
    const expected = [_]Token.Kind{ .literal, .any_character, .literal };
    try expectTokens(input, &expected);
}

test "lexer groups" {
    const input: []const u8 = "(ab)";
    const expected = [_]Token.Kind{ .open_group, .literal, .literal, .close_group };
    try expectTokens(input, &expected);
}

test "lexer zero or more" {
    const input: []const u8 = "a*";
    const expected = [_]Token.Kind{ .literal, .zero_or_more };
    try expectTokens(input, &expected);
}

test "lexer one or more" {
    const input: []const u8 = "a+";
    const expected = [_]Token.Kind{ .literal, .one_or_more };
    try expectTokens(input, &expected);
}

test "lexer zero or one" {
    const input: []const u8 = "a?";
    const expected = [_]Token.Kind{ .literal, .zero_or_one };
    try expectTokens(input, &expected);
}

test "lexer interval exact" {
    const input: []const u8 = "a{3}";

    const expected = [_]Token.Kind{ .literal, .open_interval_brace, .interval_number, .close_interval_brace };
    try expectTokens(input, &expected);
}

test "lexer interval min only" {
    const input: []const u8 = "a{3,}";
    const expected = [_]Token.Kind{ .literal, .open_interval_brace, .interval_number, .interval_comma, .close_interval_brace };
    try expectTokens(input, &expected);
}

test "lexer interval min max" {
    const input: []const u8 = "a{3,5}";
    const expected = [_]Token.Kind{ .literal, .open_interval_brace, .interval_number, .interval_comma, .interval_number, .close_interval_brace };
    try expectTokens(input, &expected);
}

test "lexer backslash escape metacharacter" {
    const input: []const u8 = "\\.\\*\\|";
    const expected = [_]Token.Kind{ .literal, .literal, .literal };
    try expectTokens(input, &expected);
}

test "lexer backslash escape literal" {
    const input: []const u8 = "\\a";
    const expected = [_]Token.Kind{.literal};
    try expectTokens(input, &expected);
}

test "lexer backslash escape backslash" {
    const input: []const u8 = "\\\\";
    const expected = [_]Token.Kind{.literal};
    try expectTokens(input, &expected);
}

test "lexer backreference" {
    const input: []const u8 = "(a)\\1";
    const expected = [_]Token.Kind{ .open_group, .literal, .close_group, .backreference };
    try expectTokens(input, &expected);
}

test "lexer simple bracket expression" {
    const input: []const u8 = "[abc]";

    const expected = [_]Token.Kind{ .open_bracket, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket with literal closing bracket first" {
    const input: []const u8 = "[]abc]";

    const expected = [_]Token.Kind{ .open_bracket, .literal, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer negated bracket expression" {
    const input: []const u8 = "[^abc]";

    const expected = [_]Token.Kind{ .open_bracket, .bracket_negation, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket with literal caret" {
    const input: []const u8 = "[a^bc]";

    const expected = [_]Token.Kind{ .open_bracket, .literal, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket range expression" {
    const input: []const u8 = "[a-z0-9]";
    const expected = [_]Token.Kind{ .open_bracket, .literal, .bracket_range_operator, .literal, .literal, .bracket_range_operator, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket with literal hyphen start" {
    const input: []const u8 = "[-abc]";

    const expected = [_]Token.Kind{ .open_bracket, .literal, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket with literal hyphen end" {
    const input: []const u8 = "[abc-]";

    const expected = [_]Token.Kind{ .open_bracket, .literal, .literal, .literal, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket character class" {
    const input: []const u8 = "[[:alnum:]a]";
    const expected = [_]Token.Kind{ .open_bracket, .character_class_start, .character_class_name, .character_class_end, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket collating element" {
    const input: []const u8 = "[[.a.]b]";
    const expected = [_]Token.Kind{ .open_bracket, .collating_element_start, .collating_element_name, .collating_element_end, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer bracket equivalence class" {
    const input: []const u8 = "[[=a=]b]";
    const expected = [_]Token.Kind{ .open_bracket, .equivalence_class_start, .equivalence_class_name, .equivalence_class_end, .literal, .close_bracket };
    try expectTokens(input, &expected);
}

test "lexer combination 1" {
    const input: []const u8 = "(a*b|c?)+";
    const expected = [_]Token.Kind{
        .open_group, .literal, .zero_or_more, .literal, .alternation, .literal, .zero_or_one, .close_group, .one_or_more,
    };
    try expectTokens(input, &expected);
}

test "lexer combination 2" {
    const input: []const u8 = "^[a-z]{1,5}$";
    const expected = [_]Token.Kind{
        .start_anchor,        .open_bracket,    .literal,        .bracket_range_operator, .literal,              .close_bracket,
        .open_interval_brace, .interval_number, .interval_comma, .interval_number,        .close_interval_brace, .end_anchor,
    };
    try expectTokens(input, &expected);
}
