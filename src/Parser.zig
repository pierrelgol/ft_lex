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
const ArrayList = std.ArrayList;
const HashMap = std.AutoArrayHashMap;

const test_inputs = @import("lexinputs");

const Auxiliaries = @import("Auxiliaries.zig");
const Declarations = @import("Declarations.zig");
const Rules = @import("Rules.zig");
const SourceLocation = @import("SourceLocation.zig");
const LocationIndex = SourceLocation.LocationIndex;

const Parser = @This();

allocator: mem.Allocator,
locations: ArrayList(SourceLocation),
location: SourceLocation,
decls: Declarations,
rules: Rules,
auxs: Auxiliaries,
state: ParsingState,

pub fn init(allocator: mem.Allocator) Parser {
    return .{
        .allocator = allocator,
        .locations = ArrayList(SourceLocation).init(allocator),
        .location = SourceLocation{},
        .decls = Declarations.init(allocator),
        .rules = Rules.init(allocator),
        .auxs = Auxiliaries.init(allocator),
        .state = .inBeginState,
    };
}

pub fn deinit(self: *Parser) void {
    self.locations.deinit();
    self.decls.deinit();
    self.rules.deinit();
    self.auxs.deinit();
    self.* = undefined;
}

pub fn parse(self: *Parser, file_name: []const u8, file_content: []const u8) !void {
    self.location.filename = file_name;
    self.location.line = 0;
    self.location.column = 0;
    var readline = mem.tokenizeScalar(u8, file_content, '\n');
    var in_comment: bool = false;

    while (readline.next()) |line| {
        if (mem.startsWith(u8, line, "/*")) {
            in_comment = true;
        } else if (mem.indexOf(u8, line, "/*")) |found| {
            std.debug.print("{s}\n", .{line[0..found]});
            in_comment = true;
        }

        if (mem.endsWith(u8, line, "*/")) {
            in_comment = false;
            continue;
        }

        if (!in_comment)
            std.debug.print("{s}\n", .{line});
    }
}

pub fn dbgState(self: *Parser, prev: ParsingState) void {
    std.debug.print("switching state {s} --> {s}\n", .{ @tagName(prev), @tagName(self.state) });
}

pub fn nextState(self: *Parser, curr: u8, next: u8) void {
    const found_block_delimiter: bool = if (curr == '%' and next == '%') true else false;
    self.state = switch (self.state) {
        .inBeginState => if (found_block_delimiter) .inRulesBlock else .inDeclarationBlock,
        .inDeclarationBlock => if (found_block_delimiter) .inRulesBlock else .inDeclarationBlock,
        .inRulesBlock => if (found_block_delimiter) .inAuxiliariesBlock else .inRulesBlock,
        .inAuxiliariesBlock => if (found_block_delimiter) .inFinishedState else .inAuxiliariesBlock,
        .inFinishedState => .inFinishedState,
    };
}

pub fn dbgChar(char: ?u8) void {
    std.debug.print("{?c}", .{char});
}

pub const ParsingState = enum {
    inBeginState,
    inDeclarationBlock,
    inRulesBlock,
    inAuxiliariesBlock,
    inFinishedState,
};

pub fn format(
    self: @This(),
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    _ = self;
    _ = writer;
}
