const std = @import("std");
const re = @import("regex.zig");
const ReAst = re.Ast;
const ReAstNode = re.Node;
const ReAstNodeKind = re.Node.Kind;
const mem = std.mem;
const heap = std.heap;
const HashMap = std.AutoArrayHashMapUnmanaged;
const Bitset256 = std.bit_set.IntegerBitSet(256);
const BoundedArray = std.BoundedArray;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

pub const Gloushkov = struct {
    arena: heap.ArenaAllocator,
    pos_map: HashMap(*ReAstNode, usize),
    pos_first: HashMap(*ReAstNode, Bitset256),
    pos_last: HashMap(*ReAstNode, Bitset256),
    nullable: HashMap(*ReAstNode, bool),
    pos_follow: BoundedArray(Bitset256, 256),
    symbols: BoundedArray(Symbol, 256),

    pub fn init(gpa: mem.Allocator) Gloushkov {
        return .{
            .arena = heap.ArenaAllocator.init(gpa),
            .pos_map = HashMap(*ReAstNode, usize).empty,
            .pos_first = HashMap(*ReAstNode, Bitset256).empty,
            .pos_last = HashMap(*ReAstNode, Bitset256).empty,
            .nullable = HashMap(*ReAstNode, bool).empty,
            .pos_follow = BoundedArray(Bitset256, 256).init(256) catch unreachable,
            .symbols = BoundedArray(Symbol, 256).init(0) catch unreachable,
        };
    }

    pub fn deinit(self: *Gloushkov) void {
        defer self.* = undefined;
        self.arena.deinit();
    }

    pub fn symbolFromNode(_: *const Gloushkov, node: *ReAstNode) Symbol {
        return switch (@as(ReAstNode, node.*)) {
            .literal => .{ .literal = node.literal },
            .class => .{ .class = node },
            .quoted => .{ .quoted = &node.quoted.string },
            else => unreachable,
        };
    }

    pub fn addSymbol(self: *Gloushkov, symbol: Symbol) void {
        self.symbols.appendAssumeCapacity(symbol);
    }

    fn getId(self: *const Gloushkov) usize {
        return self.symbols.len;
    }

    fn computePositions(self: *Gloushkov, node: *ReAstNode) !void {
        if (self.pos_map.get(node) != null) return;

        switch (node.*) {
            .literal, .class, .quoted => {
                const id = self.getId();
                try self.pos_map.put(self.arena.allocator(), node, id);
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

    fn computeNullable(self: *Gloushkov, node: *ReAstNode) bool {
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

    fn computeFirstLast(self: *Gloushkov, node: *ReAstNode) !void {
        if (self.pos_first.get(node)) |_| return;

        var fp = Bitset256.initEmpty();
        var lp = Bitset256.initEmpty();

        switch (node.*) {
            .literal, .class, .quoted => {
                const id = self.pos_map.get(node) orelse unreachable;
                fp.set(id);
                lp.set(id);
            },
            .group => |g| {
                try self.computeFirstLast(g);
                fp = self.pos_first.get(g) orelse unreachable;
                lp = self.pos_last.get(g) orelse unreachable;
            },
            .alternation => |a| {
                try self.computeFirstLast(a.lhs);
                try self.computeFirstLast(a.rhs);
                fp = self.pos_first.get(a.lhs) orelse unreachable;
                fp = fp.unionWith(self.pos_first.get(a.rhs) orelse unreachable);
                lp = self.pos_last.get(a.lhs) orelse unreachable;
                lp = lp.unionWith(self.pos_last.get(a.rhs) orelse unreachable);
            },
            .concat => |c| {
                try self.computeFirstLast(c.lhs);
                try self.computeFirstLast(c.rhs);
                const lhs_fp = self.pos_first.get(c.lhs) orelse unreachable;
                const rhs_fp = self.pos_first.get(c.rhs) orelse unreachable;
                const lhs_lp = self.pos_last.get(c.lhs) orelse unreachable;
                const rhs_lp = self.pos_last.get(c.rhs) orelse unreachable;

                if (self.computeNullable(c.lhs)) {
                    fp = lhs_fp;
                    fp = fp.unionWith(rhs_fp);
                } else {
                    fp = lhs_fp;
                }

                if (self.computeNullable(c.rhs)) {
                    lp = lhs_lp;
                    lp = lp.unionWith(rhs_lp);
                } else {
                    lp = rhs_lp;
                }
            },
            .quantifier => |q| {
                try self.computeFirstLast(q.lhs);
                fp = self.pos_first.get(q.lhs) orelse unreachable;
                lp = self.pos_last.get(q.lhs) orelse unreachable;
            },
        }

        try self.pos_first.put(self.arena.allocator(), node, fp);
        try self.pos_last.put(self.arena.allocator(), node, lp);
    }

    fn computeFollow(self: *Gloushkov, node: *ReAstNode) void {
        switch (node.*) {
            .concat => |c| {
                try self.computeFollow(c.lhs);
                try self.computeFollow(c.rhs);

                const lhs_bs = self.pos_last.get(c.lhs) orelse unreachable;
                const rhs_bs = self.pos_last.get(c.rhs) orelse unreachable;

                var lhs_it = lhs_bs.iterator(.{ .kind = .set, .direction = .forward });
                while (lhs_it.next()) |i| {
                    var rhs_it = rhs_bs.iterator(.{ .kind = .set, .direction = .forward });
                    while (rhs_it.next()) |j| {
                        self.pos_follow.buffer[i].set(j);
                    }
                }
            },
            .quantifier => |q| {
                try self.computeFollow(q.lhs);

                if (q.max) |_| {} else {
                    const lp_bs = self.pos_last.get(q.lhs) orelse unreachable;
                    const fp_bs = self.pos_first.get(q.lhs) orelse unreachable;

                    var lp_it = lp_bs.iterator(.{ .kind = .set, .direction = .forward });
                    while (lp_it.next()) |i| {
                        var fp_it = fp_bs.iterator(.{ .kind = .set, .direction = .forward });
                        while (fp_it.next()) |j| {
                            self.pos_follow.buffer[i].set(j);
                        }
                    }
                }
            },
            .alternation => |a| {
                try self.computeFollow(a.lhs);
                try self.computeFollow(a.rhs);
            },
            .group => |g| {
                try self.computeFollow(g);
            },
            else => {},
        }
    }

    pub fn glushkovFromAst(self: *Gloushkov, root: *ReAstNode) !void {
        try self.computePositions(root);
        _ = try self.computeNullable(root);
        try self.computeFirstLast(root);
        try self.computeFollow(root);
    }

    pub fn equivalenceClassFromGlushkov(self: *Gloushkov, root: *ReAstNode) !EquivalenceClass {
        try self.glushkovFromAst(root);
        const N = self.symbols.len;

        var sig: [256]Bitset256 = undefined;
        for (0..256) |c| {
            var bs = Bitset256.initEmpty();
            for (self.symbols.buffer[0..N], 0..) |sym, i| {
                if (sym.matches(@as(u8, c))) bs.set(i);
            }
            sig[c] = bs;
        }

        var cmaps = HashMap(Bitset256, u16).empty;
        var next_class: u16 = 0;
        var result: EquivalenceClass = .{};
        @memset(result.class, 0);

        for (0..256) |c| {
            const bs = sig[c];

            if (cmaps.get(bs)) |index| {
                result.class[c] = index;
            } else {
                result.class[c] = next_class;
                try cmaps.put(self.arena.allocator(), bs, next_class);
                next_class += 1;
            }
        }

        result.count = next_class;
        return result;
    }
};

pub const EquivalenceClass = struct {
    class: [256]u16 = undefined,
    count: u16 = 0,

    pub fn init() EquivalenceClass {
        return .{
            .class = &[_]u16{},
            .count = 0,
        };
    }

    pub fn addClass(self: *EquivalenceClass, index: usize, value: u16) void {
        self.class[index] = value;
        self.count += 1;
    }
};

pub const Symbol = union(enum) {
    literal: u8,
    class: *const ReAstNode,
    quoted: *const []const u8,

    pub fn matches(self: Symbol, c: u8) bool {
        return switch (self) {
            .literal => |lit| lit == c,

            .class => |nodePtr| {
                const cls = nodePtr.class;
                const in_set = cls.charset.isSet(c);
                return if (cls.negated) !in_set else in_set;
            },

            .quoted => |strPtr| {
                for (strPtr.*) |b| if (b == c) return true;
                return false;
            },
        };
    }
};
