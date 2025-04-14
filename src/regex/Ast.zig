const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const Allocator = mem.Allocator;

const Charset = std.StaticBitSet(std.math.maxInt(u8));

pub const Ast = struct {
    pool: heap.MemoryPool(Node),
    root: ?*Node,

    pub const Node = union(Kind) {
        alternation: struct {
            lhs: *Node,
            rhs: *Node,
        },

        anchor_end: *Node,

        anchor_start: *Node,

        class: struct {
            negated: bool,
            charset: Charset,
        },

        concatenation: struct {
            lhs: *Node,
            rhs: *Node,
        },

        grouping: *Node,

        literal: u8,

        quantifier: struct {
            lhs: *Node,
            min: usize,
            max: ?usize,
            greedy: bool,
        },

        trailing_context: struct {
            lhs: *Node,
            rhs: *Node,
        },

        pub const Kind = enum {
            alternation,
            anchor_end,
            anchor_start,
            class,
            concatenation,
            grouping,
            literal,
            quantifier,
            trailing_context,
        };

        pub fn init(comptime kind: Kind, init_expr: anytype) Node {
            return @unionInit(Node, @tagName(kind), init_expr);
        }

        pub fn tag(self: *const Node) Kind {
            return std.meta.activeTag(self.*);
        }
    };

    pub fn init(gpa: Allocator) Ast {
        return .{
            .pool = heap.MemoryPool(Node).init(gpa),
            .root = null,
        };
    }

    pub fn deinit(self: *Ast) void {
        defer self.* = undefined;
        self.pool.deinit();
    }

    pub fn createNode(self: *Ast) Allocator.Error!*Node {
        return self.pool.create();
    }
};
