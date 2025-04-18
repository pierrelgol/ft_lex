const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const fmt = std.fmt;
const Allocator = mem.Allocator;
const Charset = std.bit_set.IntegerBitSet(256);

pub const Ast = struct {
    mpool: heap.MemoryPool(Node) = undefined,
    arena: heap.ArenaAllocator = undefined,
    root: ?*Node = null,

    pub fn init(gpa: Allocator) Ast {
        return .{
            .mpool = heap.MemoryPool(Node).init(gpa),
            .arena = heap.ArenaAllocator.init(gpa),
            .root = null,
        };
    }

    pub fn deinit(self: *Ast) void {
        defer self.* = undefined;
        self.arena.deinit();
        self.mpool.deinit();
    }

    pub fn scratchAllocator(self: *Ast) Allocator {
        return self.arena.allocator();
    }

    pub fn createNode(self: *Ast) Allocator.Error!*Node {
        return self.mpool.create();
    }

    pub fn cloneNode(self: *Ast, original: *const Node) !*Node {
        const new_node = try self.createNode();
        switch (original.*) {
            .literal => |orig_lit| {
                new_node.* = .{ .literal = orig_lit };
            },
            .class => |orig_class| {
                new_node.* = .{ .class = orig_class };
            },
            .quoted => |orig_quoted| {
                const cloned_string = try self.scratchAllocator().dupe(u8, orig_quoted.string);
                new_node.* = .{ .quoted = .{ .string = cloned_string } };
            },
            .group => |orig_child_ptr| {
                const cloned_child = try self.cloneNode(orig_child_ptr);
                new_node.* = .{ .group = cloned_child };
            },
            .quantifier => |orig_quant| {
                const cloned_lhs = try self.cloneNode(orig_quant.lhs);

                new_node.* = .{
                    .quantifier = .{
                        .min = orig_quant.min,
                        .max = orig_quant.max,
                        .lhs = cloned_lhs,
                    },
                };
            },
            .alternation => |orig_alt| {
                const cloned_lhs = try self.cloneNode(orig_alt.lhs);
                const cloned_rhs = try self.cloneNode(orig_alt.rhs);
                new_node.* = .{ .alternation = .{
                    .lhs = cloned_lhs,
                    .rhs = cloned_rhs,
                } };
            },
            .concat => |orig_concat| {
                const cloned_lhs = try self.cloneNode(orig_concat.lhs);
                const cloned_rhs = try self.cloneNode(orig_concat.rhs);
                new_node.* = .{ .concat = .{
                    .lhs = cloned_lhs,
                    .rhs = cloned_rhs,
                } };
            },
        }

        return new_node;
    }

    pub fn cloneNullableNode(self: *Ast, original: ?*const Node) !?*Node {
        if (original) |orig_node| {
            return try self.cloneNode(orig_node);
        } else {
            return null;
        }
    }

    pub const Node = union(Kind) {
        alternation: struct {
            lhs: *Node,
            rhs: *Node,
        },
        class: struct {
            negated: bool,
            charset: Charset,
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

        pub const Kind = enum {
            alternation,
            class,
            concat,
            group,
            literal,
            quantifier,
            quoted,
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
