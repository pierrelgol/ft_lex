const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const BitSet = std.bit_set.DynamicBitSet;
const testing = std.testing;

const Ast = @import("Ast.zig").Ast;
const Lexer = @import("Lexer.zig").Lexer;
const Parser = @import("Parser.zig").Parser;
const Token = @import("Token.zig").Token;
const Charset = std.bit_set.StaticBitSet(256);

const TransitionCondition = union(enum) {
    epsilon: void,
    literal: u8,
    class: Charset,
};

const Transition = struct {
    condition: TransitionCondition,
    next_state: usize,
};

const NfaState = struct {
    id: usize,

    transitions: ArrayList(Transition),
    is_accepting: bool = false,

    pub fn init(id: usize, allocator: Allocator) !NfaState {
        return NfaState{
            .id = id,
            .transitions = ArrayList(Transition).init(allocator),
            .is_accepting = false,
        };
    }

    pub fn deinit(self: *NfaState) void {
        self.transitions.deinit();
    }

    pub fn addTransition(self: *NfaState, condition: TransitionCondition, next_state: usize) !void {
        try self.transitions.append(.{ .condition = condition, .next_state = next_state });
    }
};

pub const Nfa = struct {
    allocator: Allocator,
    states: ArrayList(NfaState),
    start_state: usize,

    pub fn init(gpa: Allocator) Nfa {
        return Nfa{
            .allocator = gpa,
            .states = ArrayList(NfaState).init(gpa),
            .start_state = 0,
        };
    }

    pub fn deinit(self: *Nfa) void {
        defer self.* = undefined;
        for (self.states.items) |*state| {
            state.deinit();
        }
        self.states.deinit();
    }

    fn addState(self: *Nfa) !usize {
        const state_id = self.states.items.len;
        try self.states.append(try NfaState.init(state_id, self.allocator));
        return state_id;
    }

    fn addEpsilonTransition(self: *Nfa, from_id: usize, to_id: usize) !void {
        try self.states.items[from_id].addTransition(.epsilon, to_id);
    }

    fn addLiteralTransition(self: *Nfa, from_id: usize, to_id: usize, char: u8) !void {
        try self.states.items[from_id].addTransition(.{ .literal = char }, to_id);
    }

    fn addClassTransition(self: *Nfa, from_id: usize, to_id: usize, charset: Charset) !void {
        try self.states.items[from_id].addTransition(.{ .class = charset }, to_id);
    }
};

const NfaFragment = struct {
    start_state: usize,
    end_state: usize,
};

const NfaBuilder = struct {
    nfa: *Nfa,
    ast: *Ast,

    fn build(self: *NfaBuilder) !void {
        if (self.ast.root) |root_node| {
            const main_fragment = try self.astNodeToFragment(root_node);
            self.nfa.start_state = main_fragment.start_state;

            if (main_fragment.end_state < self.nfa.states.items.len) {
                self.nfa.states.items[main_fragment.end_state].is_accepting = true;
            } else {
                std.log.err("End state {} out of bounds ({})", .{ main_fragment.end_state, self.nfa.states.items.len });
                return error.NfaBuildError;
            }
        } else {
            const start = try self.nfa.addState();
            const end = try self.nfa.addState();
            try self.nfa.addEpsilonTransition(start, end);
            self.nfa.start_state = start;
            if (end < self.nfa.states.items.len) {
                self.nfa.states.items[end].is_accepting = true;
            } else {
                return error.NfaBuildError;
            }
        }
    }

    fn astNodeToFragment(self: *NfaBuilder, node: *const Ast.Node) !NfaFragment {
        switch (node.*) {
            .literal => |lit| {
                const start = try self.nfa.addState();
                const end = try self.nfa.addState();
                try self.nfa.addLiteralTransition(start, end, lit);
                return NfaFragment{ .start_state = start, .end_state = end };
            },

            .class => |cls| {
                const start = try self.nfa.addState();
                const end = try self.nfa.addState();

                var effective_charset = cls.charset;
                if (cls.negated) {
                    var full_set = Charset.initFull();

                    effective_charset = full_set.differenceWith(cls.charset);
                }
                try self.nfa.addClassTransition(start, end, effective_charset);
                return NfaFragment{ .start_state = start, .end_state = end };
            },

            .quoted => |q| {
                if (q.string.len == 0) {
                    const start = try self.nfa.addState();
                    const end = try self.nfa.addState();
                    try self.nfa.addEpsilonTransition(start, end);
                    return NfaFragment{ .start_state = start, .end_state = end };
                }

                var current_fragment = try self.astNodeToFragment(&Ast.Node{ .literal = q.string[0] });

                for (q.string[1..]) |char_byte| {
                    const next_char_node = Ast.Node{ .literal = char_byte };
                    const next_fragment = try self.astNodeToFragment(&next_char_node);

                    try self.nfa.addEpsilonTransition(current_fragment.end_state, next_fragment.start_state);

                    current_fragment.end_state = next_fragment.end_state;
                }
                return current_fragment;
            },

            .concat => |cat| {
                const frag1 = try self.astNodeToFragment(cat.lhs);
                const frag2 = try self.astNodeToFragment(cat.rhs);

                try self.nfa.addEpsilonTransition(frag1.end_state, frag2.start_state);

                return NfaFragment{ .start_state = frag1.start_state, .end_state = frag2.end_state };
            },

            .alternation => |alt| {
                const frag1 = try self.astNodeToFragment(alt.lhs);
                const frag2 = try self.astNodeToFragment(alt.rhs);

                const new_start = try self.nfa.addState();
                const new_end = try self.nfa.addState();

                try self.nfa.addEpsilonTransition(new_start, frag1.start_state);
                try self.nfa.addEpsilonTransition(new_start, frag2.start_state);

                try self.nfa.addEpsilonTransition(frag1.end_state, new_end);
                try self.nfa.addEpsilonTransition(frag2.end_state, new_end);

                return NfaFragment{ .start_state = new_start, .end_state = new_end };
            },

            .group => |grp_node| {
                return try self.astNodeToFragment(grp_node);
            },

            .quantifier => |quant| {
                if (quant.min == 0 and quant.max == 1) {
                    const frag = try self.astNodeToFragment(quant.lhs);
                    const new_start = try self.nfa.addState();
                    const new_end = try self.nfa.addState();
                    try self.nfa.addEpsilonTransition(new_start, frag.start_state);
                    try self.nfa.addEpsilonTransition(frag.end_state, new_end);
                    try self.nfa.addEpsilonTransition(new_start, new_end);
                    return NfaFragment{ .start_state = new_start, .end_state = new_end };
                } else if (quant.min == 0 and quant.max == null) {
                    const frag = try self.astNodeToFragment(quant.lhs);
                    const new_start = try self.nfa.addState();
                    const new_end = try self.nfa.addState();
                    try self.nfa.addEpsilonTransition(new_start, frag.start_state);
                    try self.nfa.addEpsilonTransition(frag.end_state, frag.start_state);
                    try self.nfa.addEpsilonTransition(frag.end_state, new_end);
                    try self.nfa.addEpsilonTransition(new_start, new_end);
                    return NfaFragment{ .start_state = new_start, .end_state = new_end };
                } else if (quant.min == 1 and quant.max == null) {
                    const frag1 = try self.astNodeToFragment(quant.lhs);

                    const star_body_frag = try self.astNodeToFragment(quant.lhs);
                    const star_start = try self.nfa.addState();
                    const star_end = try self.nfa.addState();
                    try self.nfa.addEpsilonTransition(star_start, star_body_frag.start_state);
                    try self.nfa.addEpsilonTransition(star_body_frag.end_state, star_body_frag.start_state);
                    try self.nfa.addEpsilonTransition(star_body_frag.end_state, star_end);
                    try self.nfa.addEpsilonTransition(star_start, star_end);

                    try self.nfa.addEpsilonTransition(frag1.end_state, star_start);

                    return NfaFragment{ .start_state = frag1.start_state, .end_state = star_end };
                } else if (quant.max != null and quant.min == quant.max.?) {
                    const n = quant.min;
                    if (n == 0) {
                        const start = try self.nfa.addState();
                        const end = try self.nfa.addState();
                        try self.nfa.addEpsilonTransition(start, end);
                        return NfaFragment{ .start_state = start, .end_state = end };
                    }

                    var current_fragment = try self.astNodeToFragment(quant.lhs);
                    var i: usize = 1;
                    while (i < n) : (i += 1) {
                        const next_fragment = try self.astNodeToFragment(quant.lhs);
                        try self.nfa.addEpsilonTransition(current_fragment.end_state, next_fragment.start_state);
                        current_fragment.end_state = next_fragment.end_state;
                    }
                    return current_fragment;
                } else if (quant.max == null) {
                    const m = quant.min;

                    var mandatory_fragment = try self.astNodeToFragment(quant.lhs);
                    var i: usize = 1;
                    while (i < m) : (i += 1) {
                        const next_frag = try self.astNodeToFragment(quant.lhs);
                        try self.nfa.addEpsilonTransition(mandatory_fragment.end_state, next_frag.start_state);
                        mandatory_fragment.end_state = next_frag.end_state;
                    }

                    const star_body_frag_m = try self.astNodeToFragment(quant.lhs);
                    const star_start_m = try self.nfa.addState();
                    const star_end_m = try self.nfa.addState();
                    try self.nfa.addEpsilonTransition(star_start_m, star_body_frag_m.start_state);
                    try self.nfa.addEpsilonTransition(star_body_frag_m.end_state, star_body_frag_m.start_state);
                    try self.nfa.addEpsilonTransition(star_body_frag_m.end_state, star_end_m);
                    try self.nfa.addEpsilonTransition(star_start_m, star_end_m);

                    try self.nfa.addEpsilonTransition(mandatory_fragment.end_state, star_start_m);

                    return NfaFragment{ .start_state = mandatory_fragment.start_state, .end_state = star_end_m };
                } else {
                    const m = quant.min;
                    const n = quant.max.?;

                    if (m == 0) {
                        const overall_start = try self.nfa.addState();
                        var last_end = overall_start;

                        var i: usize = 0;
                        while (i < n) : (i += 1) {
                            const frag_q = try self.astNodeToFragment(quant.lhs);
                            const start_q = try self.nfa.addState();
                            const end_q = try self.nfa.addState();
                            try self.nfa.addEpsilonTransition(start_q, frag_q.start_state);
                            try self.nfa.addEpsilonTransition(frag_q.end_state, end_q);
                            try self.nfa.addEpsilonTransition(start_q, end_q);

                            try self.nfa.addEpsilonTransition(last_end, start_q);
                            last_end = end_q;
                        }

                        if (n == 0) {
                            try self.nfa.addEpsilonTransition(overall_start, last_end);
                        }

                        return NfaFragment{ .start_state = overall_start, .end_state = last_end };
                    } else {
                        var current_mandatory_fragment = try self.astNodeToFragment(quant.lhs);
                        var i_m: usize = 1;
                        while (i_m < m) : (i_m += 1) {
                            const next_frag_m = try self.astNodeToFragment(quant.lhs);
                            try self.nfa.addEpsilonTransition(current_mandatory_fragment.end_state, next_frag_m.start_state);
                            current_mandatory_fragment.end_state = next_frag_m.end_state;
                        }

                        var last_node_end = current_mandatory_fragment.end_state;
                        var i_opt: usize = 0;
                        const num_optional = n - m;
                        while (i_opt < num_optional) : (i_opt += 1) {
                            const frag_q = try self.astNodeToFragment(quant.lhs);
                            const start_q = try self.nfa.addState();
                            const end_q = try self.nfa.addState();
                            try self.nfa.addEpsilonTransition(start_q, frag_q.start_state);
                            try self.nfa.addEpsilonTransition(frag_q.end_state, end_q);
                            try self.nfa.addEpsilonTransition(start_q, end_q);

                            try self.nfa.addEpsilonTransition(last_node_end, start_q);
                            last_node_end = end_q;
                        }
                        return NfaFragment{ .start_state = current_mandatory_fragment.start_state, .end_state = last_node_end };
                    }
                }
            },

            .ends_with => |node_ptr| {
                return try self.astNodeToFragment(node_ptr);
            },
            .start_with => |node_ptr| {
                return try self.astNodeToFragment(node_ptr);
            },
            .trailing => |trail| {
                std.log.warn("Trailing context '/' NFA generation is simplified", .{});
                return try self.astNodeToFragment(trail.lhs);
            },
        }
    }
};

pub fn compileAstToNfa(ast: *Ast, gpa: Allocator) !Nfa {
    var nfa = Nfa.init(gpa);

    var builder = NfaBuilder{ .nfa = &nfa, .ast = ast };
    try builder.build();
    return nfa;
}

fn compilePattern(allocator: Allocator, pattern: []const u8) !Nfa {
    var parser = Parser.init(allocator, pattern);

    defer parser.deinit();

    _ = try parser.parse();

    const nfa = try compileAstToNfa(&parser.ast, allocator);

    return nfa;
}

test "compile empty regex" {
    const allocator = testing.allocator;
    var nfa = try compilePattern(allocator, "");
    defer nfa.deinit();

    try testing.expectEqual(@as(usize, 2), nfa.states.items.len);
    const start_state = nfa.states.items[nfa.start_state];
    try testing.expectEqual(@as(usize, 1), start_state.transitions.items.len);
    try testing.expectEqual(false, start_state.is_accepting);

    const transition = start_state.transitions.items[0];

    try testing.expect(transition.condition == .epsilon);
    const end_state_id = transition.next_state;
    try testing.expect(nfa.states.items[end_state_id].is_accepting);
    try testing.expectEqual(@as(usize, 0), nfa.states.items[end_state_id].transitions.items.len);
}

test "compile literal 'a'" {
    const allocator = testing.allocator;
    var nfa = try compilePattern(allocator, "a");
    defer nfa.deinit();

    try testing.expectEqual(@as(usize, 2), nfa.states.items.len);
    const start_state = nfa.states.items[nfa.start_state];
    try testing.expectEqual(@as(usize, 1), start_state.transitions.items.len);
    try testing.expectEqual(false, start_state.is_accepting);

    const transition = start_state.transitions.items[0];

    try testing.expect(transition.condition == .literal);
    try testing.expectEqual('a', transition.condition.literal);

    const end_state_id = transition.next_state;
    try testing.expect(nfa.states.items[end_state_id].is_accepting);
    try testing.expectEqual(@as(usize, 0), nfa.states.items[end_state_id].transitions.items.len);
}

test "compile concatenation 'ab'" {
    const allocator = testing.allocator;
    var nfa = try compilePattern(allocator, "ab");
    defer nfa.deinit();

    try testing.expectEqual(@as(usize, 4), nfa.states.items.len);
    const start_a_id = nfa.start_state;
    const state_a_start = nfa.states.items[start_a_id];
    try testing.expectEqual(false, state_a_start.is_accepting);
    try testing.expectEqual(@as(usize, 1), state_a_start.transitions.items.len);
    const trans_a = state_a_start.transitions.items[0];

    try testing.expect(trans_a.condition == .literal);
    try testing.expectEqual('a', trans_a.condition.literal);

    const end_a_id = trans_a.next_state;
    const state_a_end = nfa.states.items[end_a_id];
    try testing.expectEqual(false, state_a_end.is_accepting);
    try testing.expectEqual(@as(usize, 1), state_a_end.transitions.items.len);
    const trans_eps = state_a_end.transitions.items[0];

    try testing.expect(trans_eps.condition == .epsilon);

    const start_b_id = trans_eps.next_state;
    const state_b_start = nfa.states.items[start_b_id];
    try testing.expectEqual(false, state_b_start.is_accepting);
    try testing.expectEqual(@as(usize, 1), state_b_start.transitions.items.len);
    const trans_b = state_b_start.transitions.items[0];

    try testing.expect(trans_b.condition == .literal);
    try testing.expectEqual('b', trans_b.condition.literal);

    const end_b_id = trans_b.next_state;
    const state_b_end = nfa.states.items[end_b_id];
    try testing.expect(state_b_end.is_accepting);
    try testing.expectEqual(@as(usize, 0), state_b_end.transitions.items.len);
}

test "compile alternation 'a|b'" {
    const allocator = testing.allocator;
    var nfa = try compilePattern(allocator, "a|b");
    defer nfa.deinit();

    try testing.expectEqual(@as(usize, 6), nfa.states.items.len);
    const new_start_id = nfa.start_state;
    const state_new_start = nfa.states.items[new_start_id];
    try testing.expectEqual(false, state_new_start.is_accepting);
    try testing.expectEqual(@as(usize, 2), state_new_start.transitions.items.len);

    var accept_state_id: ?usize = null;
    for (nfa.states.items) |s| {
        if (s.is_accepting) {
            try testing.expect(accept_state_id == null);
            accept_state_id = s.id;
        }
    }
    try testing.expect(accept_state_id != null);
    const final_accept_id = accept_state_id.?;
    const state_accept = nfa.states.items[final_accept_id];
    try testing.expectEqual(@as(usize, 0), state_accept.transitions.items.len);

    var found_path_a = false;
    var found_path_b = false;

    for (state_new_start.transitions.items) |trans| {
        try testing.expect(trans.condition == .epsilon);
        const next_state = nfa.states.items[trans.next_state];

        if (next_state.transitions.items.len == 1 and
            next_state.transitions.items[0].condition == .literal and
            next_state.transitions.items[0].condition.literal == 'a')
        {
            found_path_a = true;

            const end_a_id = next_state.transitions.items[0].next_state;
            const state_end_a = nfa.states.items[end_a_id];
            try testing.expectEqual(@as(usize, 1), state_end_a.transitions.items.len);
            try testing.expect(state_end_a.transitions.items[0].condition == .epsilon);
            try testing.expectEqual(final_accept_id, state_end_a.transitions.items[0].next_state);
        } else if (next_state.transitions.items.len == 1 and
            next_state.transitions.items[0].condition == .literal and
            next_state.transitions.items[0].condition.literal == 'b')
        {
            found_path_b = true;

            const end_b_id = next_state.transitions.items[0].next_state;
            const state_end_b = nfa.states.items[end_b_id];
            try testing.expectEqual(@as(usize, 1), state_end_b.transitions.items.len);
            try testing.expect(state_end_b.transitions.items[0].condition == .epsilon);
            try testing.expectEqual(final_accept_id, state_end_b.transitions.items[0].next_state);
        }
    }
    try testing.expect(found_path_a);
    try testing.expect(found_path_b);
}

test "compile quantifier 'a?'" {
    const allocator = testing.allocator;
    var nfa = try compilePattern(allocator, "a?");
    defer nfa.deinit();

    try testing.expectEqual(@as(usize, 4), nfa.states.items.len);
    const new_start_id = nfa.start_state;
    const state_new_start = nfa.states.items[new_start_id];
    try testing.expectEqual(false, state_new_start.is_accepting);
    try testing.expectEqual(@as(usize, 2), state_new_start.transitions.items.len);

    var accept_state_id: ?usize = null;
    for (nfa.states.items) |s| {
        if (s.is_accepting) accept_state_id = s.id;
    }
    try testing.expect(accept_state_id != null);
    const final_accept_id = accept_state_id.?;

    var found_skip_path = false;
    var found_a_path_start = false;

    for (state_new_start.transitions.items) |trans| {
        try testing.expect(trans.condition == .epsilon);
        if (trans.next_state == final_accept_id) {
            found_skip_path = true;
        } else {
            const state_start_a = nfa.states.items[trans.next_state];

            if (state_start_a.transitions.items.len == 1 and
                state_start_a.transitions.items[0].condition == .literal and
                state_start_a.transitions.items[0].condition.literal == 'a')
            {
                found_a_path_start = true;
                const end_a_id = state_start_a.transitions.items[0].next_state;
                const state_end_a = nfa.states.items[end_a_id];
                try testing.expectEqual(@as(usize, 1), state_end_a.transitions.items.len);
                try testing.expect(state_end_a.transitions.items[0].condition == .epsilon);
                try testing.expectEqual(final_accept_id, state_end_a.transitions.items[0].next_state);
            }
        }
    }
    try testing.expect(found_skip_path);
    try testing.expect(found_a_path_start);
}
