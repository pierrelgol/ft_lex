// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   main.zig                                           :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 17:55:29 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 17:55:30 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const heap = std.heap;
const mem = std.mem;
const log = std.log;
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const inputs = @import("lexinputs").c99_identifiers;

pub fn main() !void {
    var base_allocator = heap.DebugAllocator(.{}){};
    defer _ = base_allocator.deinit();

    var lexer = Lexer.init(base_allocator.allocator(), .{}) catch |err| {
        log.err("Fatal error encountered : {!}", .{err});
        return;
    };
    defer lexer.deinit();

    try lexer.lex("foo", inputs[0..]);
    std.Thread.sleep(1000 * 1000 * 1000);
}
