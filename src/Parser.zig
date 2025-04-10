const std = @import("std");

const atom = @import("atom.zig");
const Atom = atom.Atom;
const AtomKind = atom.Kind;

const Iterator = @import("Iterator.zig").Iterator;
const Lexer = @import("Lexer.zig").Lexer;
const tok = @import("token.zig");
const Token = tok.Token;
const TokenKind = tok.Kind;

pub const Parser = struct {
    atoms: [MAX_ATOMS]Atom = undefined,
    in_class: bool = false,

    pub fn parse(self: *Parser, inputs: []const Token) ParseError![]const Atom {
        var it: Iterator(Token) = .init(inputs);
        var len: usize = 0;
        while (it.next()) |token| : (len += 1) {
            self.atoms[len] = switch (token) {
                .alternation => Atom.init(.alternation, {}),
                .backslash => Atom.init(.literal, token.backslash),
                .caret => Atom.init(.anchor, .start),
                .dolar => Atom.init(.anchor, .end),
                .dot => Atom.init(.any, {}),
                .digit => self.parseDigit(&it),
                .question_mark => Atom.init(.quantifier, atom.Quantifier.zero_or_one),
                .asterisk => Atom.init(.quantifier, atom.Quantifier.zero_or_more),
                .plus => Atom.init(.quantifier, atom.Quantifier.one_or_more),
                // .equal => Atom.init(., value: anytype)
            };
        }
    }

    pub fn parseDigit(self: *Parser, it: *Iterator(Token)) Atom {
        var num: usize = 0;
        while (it.peek()) |num_literal| {
            if (std.meta.activeTag(num_literal) == .digit) {
                num = (num_literal.digit - '0') + (num * 10);
                it.advance();
            } else break;
        }
    }

    pub const empty: Parser = .{
        .atoms = undefined,
        .in_class = false,
    };

    pub const MAX_ATOMS: usize = 256;
    pub const ParseError = error{ SyntaxError, UnexpectedEof };
};
