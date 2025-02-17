// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Declarations.zig                                   :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 11:05:28 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 11:05:29 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //
const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const ArrayList = std.ArrayList;
const HashMap = std.AutoArrayHashMap;
const SourceLocation = @import("SourceLocation.zig");
const LocationIndex = SourceLocation.LocationIndex;

pub const RawSourceCode = struct {
    location: LocationIndex,
    text: []const u8,
};

pub const Declaration = struct {
    location: LocationIndex,
    regex: []const u8,
};

pub const Identifier = []const u8;

const Declarations = @This();

decls: HashMap(Identifier, Declaration),
blobs: ArrayList(RawSourceCode),

pub fn init(allocator: mem.Allocator) Declarations {
    return .{
        .decls = HashMap(Identifier, Declaration).init(allocator),
        .blobs = ArrayList(RawSourceCode).init(allocator),
    };
}

pub fn deinit(self: *Declarations) void {
    self.decls.deinit();
    self.blobs.deinit();
}
