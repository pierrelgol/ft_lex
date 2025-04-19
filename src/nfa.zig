const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const Allocator = std.mem.Allocator;
const regex = @import("regex.zig");
const Parser = regex.Parser;
const Ast = regex.Ast;
const AstNode = regex.Node;
const LinkedList = std.SinglyLinkedList;
const Node = LinkedList.Node;
const ArrayList = std.ArrayListUnmanaged;
const assert = std.debug.assert;

pub const Symbol = union(Kind) {
    epsilon,
    char: u8,

    fn eql(s1: Symbol, s2: Symbol) bool {
        if (s1.kind() != s2.kind()) return false;
        return switch (s1) {
            .epsilon => true,
            .char => s1.char == s2.char,
        };
    }

    fn kind(self: Symbol) Kind {
        return std.meta.activeTag(self);
    }

    pub const Kind = enum {
        epsilon,
        char,
    };

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .epsilon => try writer.print("ε", .{}),
            .char => |c| try writer.print("{c}", .{c}),
        }
    }
};

const Edge = struct {
    from: usize,
    sym: Symbol,
    to: usize,
};

const Frag = struct {
    start: usize,
    accept: usize,
};

pub const NfaBuilder = struct {
    allocator: *Allocator,
    edges: std.ArrayList(Edge),
    numStates: usize,

    pub fn init(alloc: *Allocator) NfaBuilder {
        return .{
            .allocator = alloc,
            .edges = std.ArrayList(Edge).init(alloc),
            .numStates = 0,
        };
    }

    pub fn addState(self: *NfaBuilder) usize {
        const id = self.numStates;
        self.numStates += 1;
        return id;
    }

    pub fn addEdge(self: *NfaBuilder, from: usize, sym: Symbol, to: usize) !void {
        try self.edges.append(self.allocator, Edge{ .from = from, .sym = sym, .to = to });
    }

    pub fn nfaFromAst(self: *NfaBuilder, node: *AstNode) !Frag {
        return switch (node.*) {
            .literal => |lit| {
                const s = self.addState();
                const t = self.addState();
                try self.addEdge(s, Symbol{ .char = lit }, t);
                return Frag{ .start = s, .accept = t };
            },
            .concat => |c| {
                const f1 = try self.nfaFromAst(c.lhs);
                const f2 = try self.nfaFromAst(c.rhs);

                try self.addEdge(f1.accept, Symbol.epsilon, f2.start);
                return Frag{ .start = f1.start, .accept = f2.accept };
            },
            else => unreachable,
        };
    }

    pub fn build(self: *NfaBuilder, frag: Frag) !Nfa {
        const n = self.numStates;

        var counts = std.ArrayList(usize).init(self.allocator);
        try counts.resize(self.allocator, n, 0);
        for (self.edges.items) |e| counts.items[e.from] += 1;

        var offsets = std.ArrayList(usize).init(self.allocator);
        var sum: usize = 0;
        try offsets.append(0);
        for (counts.items) |c| {
            sum += c;
            try offsets.append(sum);
        }

        var symbols = std.ArrayList(Symbol).init(self.allocator);
        var dests = std.ArrayList(usize).init(self.allocator);
        try symbols.resize(self.allocator, sum, Symbol.epsilon);
        try dests.resize(self.allocator, sum, 0);

        var next = std.ArrayList(usize).init(self.allocator);
        try next.appendAll(self.allocator, offsets.items[0..n]);

        for (self.edges.items) |e| {
            const idx = next.items[e.from];
            symbols.items[idx] = e.sym;
            dests.items[idx] = e.to;
            next.items[e.from] += 1;
        }

        next.deinit(self.allocator);
        counts.deinit(self.allocator);

        return Nfa{
            .allocator = self.allocator,
            .symbols = symbols,
            .dests = dests,
            .offsets = offsets,
            .total_states = n,
            .start = frag.start,
            .accept = frag.accept,
        };
    }
};

pub const Nfa = struct {
    allocator: *Allocator,
    symbols: std.ArrayList(Symbol),
    dests: std.ArrayList(usize),
    offsets: std.ArrayList(usize),
    total_states: usize,
    start: usize,
    accept: usize,

    pub fn deinit(self: *Nfa) void {
        self.offsets.deinit(self.allocator);
        self.dests.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
    }

    pub fn transitions(self: *Nfa, state: usize) TransitionSlice {
        const s0 = self.offsets.items[state];
        const s1 = self.offsets.items[state + 1];
        return TransitionSlice{
            .symbols = self.symbols.items[s0..s1],
            .dests = self.dests.items[s0..s1],
        };
    }

    pub fn stringify(self: *Nfa, alloc: std.mem.Allocator) ![]u8 {
        var visited = std.AutoHashMap(usize, bool).init(alloc);
        var stack = std.ArrayList(usize).init(alloc);
        var buf = std.ArrayList(u8).init(alloc);
        defer {
            visited.deinit();
            stack.deinit();

            buf.deinit();
        }
        var writer = buf.writer();
        try stack.append(self.start);
        while (stack.pop()) |stateId| {
            if (visited.contains(stateId)) continue;
            try visited.put(stateId, true);

            const ts = self.transitions(stateId);
            for (ts.symbols, 0..) |sym, i| {
                const destId = ts.dests[i];

                switch (sym.kind()) {
                    .char => |c| {
                        try writer.print("n{d} -> n{d} [label=\"{c}\"]\n", .{ stateId, destId, c });
                    },
                    .epsilon => {
                        try writer.print("n{d} -> n{d} [label=\"ε\" style=dashed]\n", .{ stateId, destId });
                    },
                }

                try stack.append(destId);
            }
        }

        try writer.print("n{d} [shape=\"doublecircle\"]\n", .{self.accept});

        return buf.toOwnedSlice();
    }
};

pub const TransitionSlice = struct {
    symbols: []Symbol,
    dests: []usize,
};
