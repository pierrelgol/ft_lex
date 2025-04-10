const std = @import("std");
const tok = @import("token.zig");
const Token = tok.Token;
const TokenKind = tok.Kind;

pub const AstNode = struct {
    lhs: ?*AstNode = null,
    rhs: ?*AstNode = null,
};

pub const Kind = enum {};
