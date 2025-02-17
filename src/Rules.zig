// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Rules.zig                                          :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 11:05:39 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 11:05:39 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const ArrayList = std.ArrayList;

const SourceLocation = @import("SourceLocation.zig");
const LocationIndex = SourceLocation.LocationIndex;

const Rules = @This();

arena: heap.ArenaAllocator,
rules: ArrayList(Rule),

pub fn init(allocator: mem.Allocator) Rules {
    var arena: heap.ArenaAllocator = .init(allocator);
    return .{
        .arena = arena,
        .rules = ArrayList(Rule).init(arena.allocator()),
    };
}

pub fn deinit(self: *Rules) void {
    self.arena.deinit();
    self.rules.deinit();
}

pub const Rule = struct {
    location: LocationIndex,
    allocator: mem.Allocator,
    ast: AST,
    action: ?Action,

    pub fn init(allocator: mem.Allocator, location: LocationIndex) Rule {
        return .{
            .allocator = allocator,
            .location = location,
            .ast = AST.init(),
            .action = null,
        };
    }

    pub fn deinit(self: *Rule) void {
        self.ast.deinit();
    }

    pub const AST = struct {
        root: ?Node,

        pub fn init() AST {
            return .{
                .root = null,
            };
        }

        pub fn deinit(self: *AST) void {
            if (self.root) |node| {
                switch (node.kind) {
                    .Literal => {},
                    .Concat => {},
                    .Alternation => {},
                    .Repetition => {},
                    .CharacterClass => {},
                    .Group => {},
                    .NamedReference => {},
                    .Anchor => {},
                }
            }
        }

        pub const Node = struct {
            kind: Kind,
            usize: LocationIndex,
            value: Value,
        };

        pub const Kind = enum {
            Literal,
            Concat,
            Alternation,
            Repetition,
            CharacterClass,
            Group,
            NamedReference,
            Anchor,
        };

        pub const Value = union(Kind) {
            Literal: struct {
                text: []const u8,
            },

            Concat: struct {
                left: *Node,
                right: *Node,
            },

            Alternation: struct {
                left: *Node,
                right: *Node,
            },

            Repetition: struct {
                node: *Node,
                min: usize,
                max: ?usize,
            },

            CharacterClass: struct {
                characters: []const u8,
                negated: bool,
            },

            Group: struct {
                node: *Node,
            },

            NamedReference: struct {
                identifier: []const u8,
            },

            Anchor: struct {
                position: AnchorType,
            },
        };

        pub const AnchorType = enum {
            Begin,
            End,
        };
    };

    pub const Action = struct {
        location: LocationIndex,
        code: []const u8,

        pub fn init(location: LocationIndex, code: []const u8) Action {
            return .{
                .location = location,
                .code = code,
            };
        }
    };
};
