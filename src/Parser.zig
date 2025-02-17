// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Parser.zig                                         :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/16 21:01:28 by pollivie          #+#    #+#             //
//   Updated: 2025/02/16 21:01:28 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const test_inputs = @import("lexinputs");
const ArrayList = std.ArrayList;
const HashMap = std.AutoArrayHashMap;
const Parser = @This();

allocator: mem.Allocator,
reader: std.io.AnyReader,
writer: std.io.AnyWriter,

locations: ArrayList(SourceLocation),
location: SourceLocation,
decls: Declarations,
rules: Rules,
auxs: Auxiliaries,

pub fn init(allocator: mem.Allocator, file: std.fs.File) Parser {
    return .{
        .allocator = allocator,
        .reader = file.reader().any(),
        .writer = file.writer().any(),
        .locations = ArrayList(SourceLocation).init(allocator),
        .location = SourceLocation{},
        .decls = Declarations.init(allocator),
        .rules = Rules.init(allocator),
        .auxs = Auxiliaries.init(allocator),
    };
}

pub fn deinit(self: *Parser) void {
    self.locations.deinit();
    self.decls.deinit();
    self.rules.deinit();
    self.auxs.deinit();
    self.* = undefined;
}

pub const LocationIndex = usize;

pub const Declarations = struct {
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

    pub const RawSourceCode = struct {
        location: LocationIndex,
        text: []const u8,
    };

    pub const Declaration = struct {
        location: LocationIndex,
        regex: []const u8,
    };

    pub const Identifier = []const u8;
};

pub const Rules = struct {
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
};

pub const Auxiliaries = struct {
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
};

pub const SourceLocation = struct {
    filename: []const u8 = "",
    line: usize = 0,
    column: usize = 0,

    pub fn init(filename: []const u8, line: usize, column: usize) SourceLocation {
        return .{
            .filename = filename,
            .line = line,
            .column = column,
        };
    }
};
