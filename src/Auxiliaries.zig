// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Auxiliaries.zig                                    :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 11:05:45 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 11:05:46 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const ArrayList = std.ArrayList;

const Auxiliaries = @This();

arena: heap.ArenaAllocator,
blobs: ArrayList([]const u8),

pub fn init(allocator: mem.Allocator) Auxiliaries {
    var arena: heap.ArenaAllocator = .init(allocator);
    return .{
        .arena = arena,
        .blobs = ArrayList([]const u8).init(arena.allocator()),
    };
}

pub fn deinit(self: *Auxiliaries) void {
    self.arena.deinit();
    self.* = undefined;
}
