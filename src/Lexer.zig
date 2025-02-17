// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Lexer.zig                                          :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 17:55:55 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 17:55:56 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const Buffer = std.ArrayListUnmanaged(u8);
const List = std.SinglyLinkedList;
const Token = @import("Token.zig");
const SourceLocation = @import("SourceLocation.zig");
const Lexer = @This();

allocator: mem.Allocator,
node_pool: heap.MemoryPool(List(Token).Node),
token_list: List(Token),
source_code: Buffer,

pub const LexerOptions = struct {
    pool_size: usize = 128,
};

pub fn init(allocator: mem.Allocator, options: LexerOptions) !Lexer {
    return .{
        .allocator = allocator,
        .token_list = List(Token){},
        .node_pool = try heap.MemoryPool(List(Token).Node).initPreheated(allocator, options.pool_size),
        .source_code = Buffer.empty,
    };
}

pub fn deinit(self: *Lexer) void {
    self.node_pool.deinit();
    self.source_code.deinit(self.allocator);
    self.* = undefined;
}

pub fn lex(self: *Lexer, source_file: []const u8, source_code: []const u8) !void {
    var location: SourceLocation = .{
        .filename = source_file,
        .line = 0,
        .column = 0,
    };
    _ = self;

    var readline = mem.tokenizeScalar(u8, source_code, '\n');
    while (readline.next()) |line| {
        defer location.nextLine();

        var tokenizer = mem.splitAny(u8, line, " \t");
        while (tokenizer.next()) |token| {
            if (token.len != 0) {
                std.debug.print("{}'{s}'\n", .{ location, token });
            }
            location.moveCol(if (token.len == 0) 1 else token.len);
        }
        std.debug.print("\n", .{});
    }
}

pub const State = enum {
    start,
    end,
    found_block_sep,
    found_comment_begin,
    in_comment,
    found_comment_end,
    found_raw_code_begin,
    in_raw_code,
    found_raw_code_ending,
};
