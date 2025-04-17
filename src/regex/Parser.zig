const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.assert;
const Allocator = mem.Allocator;
const IntegerBitSet = std.bit_set.IntegerBitSet;

pub const node = @sizeOf(Node);

pub const Parser = struct {
    arena: heap.ArenaAllocator,
    nodes: heap.MemoryPool(Node),
    input: []const u8,
    index: usize,
    token: u8,
    regex: ?*Node,

    pub fn init(gpa: Allocator, pattern: []const u8) Error!Parser {
        assert(pattern.len > 0 and pattern.len <= 256);
        var arena: heap.ArenaAllocator = .init(gpa);
        errdefer arena.deinit();

        var nodes = try heap.MemoryPool(Node).initPreheated(gpa, 256);
        errdefer nodes.deinit();

        const input = try arena.dupe(pattern);
        return .{
            .arena = arena,
            .nodes = nodes,
            .regex = input,
            .token = input[0],
            .index = 0,
        };
    }

    pub fn deinit(self: *Parser) void {
        defer self.* = undefined;
        self.arena.deinit();
        self.nodes.deinit();
    }

    pub fn next(self: *Parser) u8 {}

    pub fn parse(self: *Parser) Error!void {
        _ = self;
    }

    pub const Error = error{} || Allocator.Error;
};

pub const Node = union(enum) {
    alternation: struct {
        lhs: *Node,
        rhs: *Node,
    },
    class: struct {
        negated: bool,
        charset: IntegerBitSet,
    },
    concat: struct {
        lhs: *Node,
        rhs: *Node,
    },
    group: *Node,
    literal: u8,
    quantifier: struct {
        min: usize,
        max: ?usize,
        lhs: *Node,
    },
    quoted: struct {
        string: []const u8,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .alternation => |alt| {
                try writer.print("(alternation {any} {any})", .{ alt.lhs, alt.rhs });
            },
            .class => |cls| {
                const negated_str = if (cls.negated) "#t" else "#f";
                try writer.print("(class :negated {s} :count {d})", .{ negated_str, cls.charset.count() });
            },
            .concat => |cat| {
                try writer.print("(concat {any} {any})", .{ cat.lhs, cat.rhs });
            },
            .group => |grp| {
                try writer.print("(group {any})", .{grp});
            },
            .literal => |lit| {
                try writer.writeAll("(literal '");
                switch (lit) {
                    '\\' => try writer.writeAll("escaped"),
                    '\'' => try writer.writeAll("squote"),
                    '\n' => try writer.writeAll("newline"),
                    '\r' => try writer.writeAll("carriage"),
                    '\t' => try writer.writeAll("tab"),
                    else => try writer.print("{c}", .{lit}),
                }
                try writer.writeAll("')");
            },
            .quantifier => |quant| {
                try writer.print("(quantifier :min {} :max ", .{quant.min});
                if (quant.max) |m| {
                    try writer.print("{}", .{m});
                } else {
                    try writer.writeAll("null");
                }
                try writer.print(" {any})", .{quant.lhs});
            },
            .quoted => |quote| {
                try writer.print("(quoted {s})", .{quote.string});
            },
        }
    }
};
