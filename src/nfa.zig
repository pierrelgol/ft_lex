const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const Allocator = std.mem.Allocator;
const regex = @import("regex.zig");
const Parser = regex.Parser;
const Ast = regex.Ast;
const AstNode = regex.Node;
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
            .char => try writer.print("{c}", .{self.char}),
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
    allocator: Allocator,
    edges: std.ArrayList(Edge),
    numStates: usize,

    pub fn init(alloc: Allocator) NfaBuilder {
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
        try self.edges.append(Edge{ .from = from, .sym = sym, .to = to });
    }

    pub fn nfaFromAst(self: *NfaBuilder, node: *AstNode) !Frag {
        return switch (node.*) {
            .literal => |lit| {
                const start = self.addState();
                const accept = self.addState();
                try self.addEdge(start, Symbol{ .char = lit }, accept);
                return Frag{ .start = start, .accept = accept };
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
        try counts.ensureTotalCapacityPrecise(n);

        for (0..n) |_| try counts.append(0);

        for (self.edges.items) |e| counts.items[e.from] += 1;

        var offsets = std.ArrayList(usize).init(self.allocator);
        try offsets.ensureTotalCapacityPrecise(n + 1);
        var sum: usize = 0;
        try offsets.append(sum);
        for (counts.items) |c| {
            sum += c;
            try offsets.append(sum);
        }

        var symbols = std.ArrayList(Symbol).init(self.allocator);
        var dests = std.ArrayList(usize).init(self.allocator);

        try symbols.resize(sum);
        try dests.resize(sum);

        var next = std.ArrayList(usize).init(self.allocator);
        try next.ensureTotalCapacityPrecise(n);

        for (0..n) |i| try next.append(offsets.items[i]);

        for (self.edges.items) |e| {
            const idx = next.items[e.from];
            symbols.items[idx] = e.sym;
            dests.items[idx] = e.to;
            next.items[e.from] += 1;
        }

        next.deinit();
        counts.deinit();

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
    allocator: Allocator,
    symbols: std.ArrayList(Symbol),
    dests: std.ArrayList(usize),
    offsets: std.ArrayList(usize),
    total_states: usize,
    start: usize,
    accept: usize,

    pub fn deinit(self: *Nfa) void {
        self.offsets.deinit();
        self.dests.deinit();
        self.symbols.deinit();
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
                    .char => {
                        try writer.print("n{d} -> n{d} [label=\"{c}\"]\n", .{ stateId, destId, sym.char });
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

test "simple" {
    const gpa = std.testing.allocator;
    const pattern: []const u8 = "abc";

    var arena: heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    var parser: Parser = .init(gpa, pattern);
    defer parser.deinit();

    const ast = try parser.parse();
    const root = ast.root.?;

    var nfabuilder = NfaBuilder.init(arena.allocator());
    const frag = try nfabuilder.nfaFromAst(root);
    var nfa = try nfabuilder.build(frag);
    defer nfa.deinit();

    const str = try nfa.stringify(gpa);
    defer gpa.free(str);
    std.debug.print("\n{s}", .{str});
}
