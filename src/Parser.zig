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
const Scanner = @import("Scanner.zig");

const Parser = @This();

allocator: mem.Allocator,
scanner: Scanner,
locations: ArrayList(SourceLocation),
location: SourceLocation,
decls: Declarations,
rules: Rules,
auxs: Auxiliaries,
state: ParsingState,

pub fn init(allocator: mem.Allocator, scanner: Scanner) Parser {
    return .{
        .allocator = allocator,
        .scanner = scanner,
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

pub fn parse(self: *Parser) !void {
    var scanner: *Scanner = &self.scanner;
    var curr: u8 = scanner.curr() orelse return error.EmptyFile;
    var next: u8 = scanner.peek() orelse return error.EmptyFile;

    while (scanner.curr()) |char| {
        defer scanner.advance();
        curr = char;
        next = scanner.peek() orelse curr;

        switch (self.state) {
            .inBeginState => {
                if (self.skipComments()) {
                    std.debug.print("Comment skipped\n", .{});
                    continue;
                }

                if (curr == '%' and next == '%') {
                    std.debug.print("switching state {s} --> {s}\n", .{ @tagName(self.state), @tagName(ParsingState.inRulesBlock) });
                    self.state = .inRulesBlock;
                } else {
                    std.debug.print("switching state {s} --> {s}\n", .{ @tagName(self.state), @tagName(ParsingState.inDeclarationBlock) });
                    self.state = .inDeclarationBlock;
                }
            },
            .inDeclarationBlock => {
                if (self.skipComments()) {
                    std.debug.print("Comment skipped\n", .{});
                    continue;
                }

                if (curr == '%' and next == '%') {
                    std.debug.print("switching state {s} --> {s}\n", .{ @tagName(self.state), @tagName(ParsingState.inRulesBlock) });
                    self.state = .inRulesBlock;
                }
            },
            .inRulesBlock => {
                if (curr == '%' and next == '%') {
                    std.debug.print("switching state {s} --> {s}\n", .{ @tagName(self.state), @tagName(ParsingState.inAuxiliariesBlock) });
                    self.state = .inAuxiliariesBlock;
                }
            },
            .inAuxiliariesBlock => {
                if (self.skipComments()) {
                    std.debug.print("Comment skipped\n", .{});
                    continue;
                }
            },
            else => break,
        }

        dbgChar(scanner.curr());
    }

    if (scanner.eof()) {
        self.state = .inFinishedState;
    }
}

pub fn dbgChar(char: ?u8) void {
    std.debug.print("{?c}", .{char});
}

pub fn skipComments(self: *Parser) bool {
    var scanner: *Scanner = &self.scanner;
    if (isCommentStart(scanner)) {
        while (!scanner.eof()) {
            if (isCommentEnd(scanner)) {
                eatComment(scanner);
                return true;
            } else {
                scanner.advance();
            }
        }
    }
    return false;
}

pub fn skipWhitespace(self: *Parser) void {
    _ = self.scanner.skip(std.ascii.isWhitespace);
}

fn isCommentStart(scanner: *Scanner) bool {
    const curr = scanner.curr() orelse return false;
    const next = scanner.peek() orelse return false;
    return (curr == '/' and next == '*');
}

fn eatComment(scanner: *Scanner) void {
    if (scanner.curr()) |char| {
        if (char == '/' or char == '*')
            scanner.advance()
        else
            return;
    }
    if (scanner.curr()) |char| {
        if (char == '/' or char == '*')
            scanner.advance()
        else
            return;
    }
}

fn isCommentEnd(scanner: *Scanner) bool {
    const curr = scanner.curr() orelse return false;
    const next = scanner.peek() orelse return false;
    return (curr == '*' and next == '/');
}

pub const ParsingState = enum {
    inBeginState,
    inDeclarationBlock,
    inRulesBlock,
    inAuxiliariesBlock,
    inFinishedState,
};
