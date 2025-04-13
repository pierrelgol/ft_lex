const std = @import("std");
pub const Token = @import("Token.zig").Token;
const Lexer = @This();

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    nextFn: *const fn (ctx: *anyopaque) Token,
    peekFn: *const fn (ctx: *anyopaque) Token,
};

pub fn next(l: Lexer) Token {
    return l.vtable.nextFn(l.ptr);
}

pub fn peek(l: Lexer) Token {
    return l.vtable.peekFn(l.ptr);
}
