const std = @import("std");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const mem = std.mem;
const heap = std.heap;
const HashMap = std.AutoArrayHashMapUnmanaged;
const Bitset256 = std.bit_set.IntegerBitSet(256);
const BoundedArray = std.BoundedArray;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

pub const Glushkov = struct {
    arena: heap.ArenaAllocator,
    position_map: HashMap(*const Node, usize),
    position_first: HashMap(*const Node, Bitset256),
    position_last: HashMap(*const Node, Bitset256),
    nullable: HashMap(*const Node, bool),
    position_follow: BoundedArray(Bitset256, 256),
    symbols: BoundedArray(Symbol, 256),

    pub fn init(gpa: mem.Allocator) Glushkov {
        return .{
            .arena = heap.ArenaAllocator.init(gpa),
            .position_map = HashMap(*const Node, usize).empty,
            .position_first = HashMap(*const Node, Bitset256).empty,
            .position_last = HashMap(*const Node, Bitset256).empty,
            .nullable = HashMap(*const Node, bool).empty,
            .position_follow = BoundedArray(Bitset256, 256).init(256) catch unreachable,
            .symbols = BoundedArray(Symbol, 256).init(0) catch unreachable,
        };
    }

    pub fn deinit(self: *Glushkov) void {
        defer self.* = undefined;
        self.arena.deinit();
    }

    pub fn symbolFromNode(_: *const Glushkov, node: *const Node) Symbol {
        return switch (@as(Node, node.*)) {
            .literal => .{ .literal = node.literal },
            .class => .{ .class = node },
            .quoted => .{ .quoted = &node.quoted.string },
            else => unreachable,
        };
    }

    pub fn addSymbol(self: *Glushkov, symbol: Symbol) void {
        self.symbols.appendAssumeCapacity(symbol);
    }

    fn getId(self: *const Glushkov) usize {
        return self.symbols.len;
    }

    fn computePositions(self: *Glushkov, node: *const Node) !void {
        if (self.position_map.get(node) != null) return;

        switch (node.*) {
            .literal, .class, .quoted => {
                const id = self.getId();
                try self.position_map.put(self.arena.allocator(), node, id);
                try self.symbols.append(self.symbolFromNode(node));
            },
            .group => try self.computePositions(node.group),
            .concat => |c| {
                try self.computePositions(c.lhs);
                try self.computePositions(c.rhs);
            },
            .alternation => |a| {
                try self.computePositions(a.lhs);
                try self.computePositions(a.rhs);
            },
            .quantifier => |q| {
                try self.computePositions(q.lhs);
            },
        }
    }

    fn computeNullable(self: *Glushkov, node: *const Node) !bool {
        if (self.nullable.get(node)) |found| {
            return found;
        }

        const result = switch (node.*) {
            .literal, .class, .quoted => false,
            .group => |g| try self.computeNullable(g),
            .alternation => |a| try self.computeNullable(a.lhs) or try self.computeNullable(a.rhs),
            .concat => |c| try self.computeNullable(c.lhs) and try self.computeNullable(c.rhs),
            .quantifier => |q| q.min == 0 or try self.computeNullable(q.lhs),
        };

        try self.nullable.put(self.arena.allocator(), node, result);
        return result;
    }

    fn computeFirstLast(self: *Glushkov, node: *const Node) !void {
        if (self.position_first.get(node)) |_| return;

        var fp = Bitset256.initEmpty();
        var lp = Bitset256.initEmpty();

        switch (node.*) {
            .literal, .class, .quoted => {
                const id = self.position_map.get(node) orelse unreachable;
                fp.set(id);
                lp.set(id);
            },
            .group => |g| {
                try self.computeFirstLast(g);
                fp = self.position_first.get(g) orelse unreachable;
                lp = self.position_last.get(g) orelse unreachable;
            },
            .alternation => |a| {
                try self.computeFirstLast(a.lhs);
                try self.computeFirstLast(a.rhs);
                fp = self.position_first.get(a.lhs) orelse unreachable;
                fp = fp.unionWith(self.position_first.get(a.rhs) orelse unreachable);
                lp = self.position_last.get(a.lhs) orelse unreachable;
                lp = lp.unionWith(self.position_last.get(a.rhs) orelse unreachable);
            },
            .concat => |c| {
                try self.computeFirstLast(c.lhs);
                try self.computeFirstLast(c.rhs);
                const lhs_fp = self.position_first.get(c.lhs) orelse unreachable;
                const rhs_fp = self.position_first.get(c.rhs) orelse unreachable;
                const lhs_lp = self.position_last.get(c.lhs) orelse unreachable;
                const rhs_lp = self.position_last.get(c.rhs) orelse unreachable;

                if (try self.computeNullable(c.lhs)) {
                    fp = lhs_fp;
                    fp = fp.unionWith(rhs_fp);
                } else {
                    fp = lhs_fp;
                }

                if (try self.computeNullable(c.rhs)) {
                    lp = lhs_lp;
                    lp = lp.unionWith(rhs_lp);
                } else {
                    lp = rhs_lp;
                }
            },
            .quantifier => |q| {
                try self.computeFirstLast(q.lhs);
                fp = self.position_first.get(q.lhs) orelse unreachable;
                lp = self.position_last.get(q.lhs) orelse unreachable;
            },
        }

        try self.position_first.put(self.arena.allocator(), node, fp);
        try self.position_last.put(self.arena.allocator(), node, lp);
    }

    fn computeFollow(self: *Glushkov, node: *const Node) void {
        switch (node.*) {
            .concat => |c| {
                self.computeFollow(c.lhs);
                self.computeFollow(c.rhs);

                const lhs_bs = self.position_last.get(c.lhs) orelse unreachable;
                const rhs_bs = self.position_last.get(c.rhs) orelse unreachable;

                var lhs_it = lhs_bs.iterator(.{ .kind = .set, .direction = .forward });
                while (lhs_it.next()) |i| {
                    var rhs_it = rhs_bs.iterator(.{ .kind = .set, .direction = .forward });
                    while (rhs_it.next()) |j| {
                        self.position_follow.buffer[i].set(j);
                    }
                }
            },
            .quantifier => |q| {
                self.computeFollow(q.lhs);

                if (q.max) |_| {} else {
                    const lp_bs = self.position_last.get(q.lhs) orelse unreachable;
                    const fp_bs = self.position_first.get(q.lhs) orelse unreachable;

                    var lp_it = lp_bs.iterator(.{ .kind = .set, .direction = .forward });
                    while (lp_it.next()) |i| {
                        var fp_it = fp_bs.iterator(.{ .kind = .set, .direction = .forward });
                        while (fp_it.next()) |j| {
                            self.position_follow.buffer[i].set(j);
                        }
                    }
                }
            },
            .alternation => |a| {
                self.computeFollow(a.lhs);
                self.computeFollow(a.rhs);
            },
            .group => |g| {
                self.computeFollow(g);
            },
            else => {},
        }
    }

    pub fn glushkovFromAst(self: *Glushkov, root: *const Node) !void {
        try self.computePositions(root);
        _ = try self.computeNullable(root);
        try self.computeFirstLast(root);
        self.computeFollow(root);
    }
};

pub const Symbol = union(enum) {
    literal: u8,
    class: *const Node,
    quoted: *const []const u8,

    pub fn matches(self: Symbol, char: u8) bool {
        return switch (self) {
            .literal => |literal| literal == char,

            .class => |node_ptr| {
                const class = node_ptr.class;
                const in_set = class.charset.isSet(char);
                return if (class.negated) !in_set else in_set;
            },

            .quoted => |string_ptr| {
                for (string_ptr.*) |b| if (b == char) return true;
                return false;
            },
        };
    }
};
