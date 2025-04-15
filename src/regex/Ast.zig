const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const fmt = std.fmt;
const Allocator = mem.Allocator;
const Charset = std.bit_set.StaticBitSet(256);

pub const Ast = struct {
    pool: heap.MemoryPool(Node) = undefined,
    arena: heap.ArenaAllocator = undefined,
    root: ?*Node = null,

    pub fn init(gpa: Allocator) Ast {
        return .{
            .pool = heap.MemoryPool(Node).init(gpa),
            .arena = heap.ArenaAllocator.init(gpa),
            .root = null,
        };
    }

    pub fn deinit(self: *Ast) void {
        defer self.* = undefined;
        self.arena.deinit();
        self.pool.deinit();
    }

    pub fn scratchAllocator(self: *Ast) Allocator {
        return self.arena.allocator();
    }

    pub fn createNode(self: *Ast) Allocator.Error!*Node {
        return self.pool.create();
    }

    pub const Node = union(Kind) {
        alternation: struct {
            lhs: *Node,
            rhs: *Node,
        },
        start_with: *Node,
        ends_with: *Node,
        class: struct {
            negated: bool,
            charset: Charset,
        },
        concat: struct {
            lhs: *Node,
            rhs: *Node,
        },
        condition: struct {
            cond: [64:0]u8,
            rhs: *Node,
        },
        group: *Node,
        literal: u8,
        quantifier: struct {
            min: usize,
            max: ?usize,
            lhs: *Node,
        },
        trailing: struct {
            lhs: *Node,
            rhs: *Node,
        },

        pub const Kind = enum {
            alternation,
            start_with,
            ends_with,
            class,
            concat,
            condition,
            group,
            literal,
            quantifier,
            trailing,
        };

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
                .start_with => |sw| {
                    try writer.print("(start_with {any})", .{sw});
                },
                .ends_with => |ew| {
                    try writer.print("(ends_with {any})", .{ew});
                },
                .class => |cls| {
                    const negated_str = if (cls.negated) "#t" else "#f";
                    try writer.print("(class :negated {s} :count {d})", .{ negated_str, cls.charset.count() });
                },
                .concat => |cat| {
                    try writer.print("(concat {any} {any})", .{ cat.lhs, cat.rhs });
                },
                .condition => |cond| {
                    const cond_name = mem.sliceTo(cond.cond[0..], 0);
                    try writer.print("(condition :name \"{s}\" {any})", .{ cond_name, cond.rhs });
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
                        '\t' => try writer.writeAll("tab'"),
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
                .trailing => |trail| {
                    try writer.print("(trailing {any} {any})", .{ trail.lhs, trail.rhs });
                },
            }
        }
    };

    pub fn format(
        self: @This(),
        comptime fmt_ignored: []const u8,
        options_ignored: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_ignored;
        _ = options_ignored;

        try writer.print("{any}", .{self.root});
    }
};
