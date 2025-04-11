const std = @import("std");
const BitSet = std.StaticBitSet(std.math.maxInt(u8));
const Ast = @This();

pub const Node = union(Kind) {
    alternation: struct {
        lhs: *Node,
        rhs: *Node,
    },
    anchor_end: *Node,
    anchor_start: *Node,
    character: u8,
    class: struct {
        negated: bool,
        value: BitSet,
    },
    concatenation: struct {
        lhs: *Node,
        rhs: *Node,
    },
    quantifier: struct {
        min: usize,
        max: ?usize,
        greedy: bool,
        lhs: *Node,
    },
    wildcard: void,

    pub const Kind = enum {
        alternation,
        anchor_end,
        anchor_start,
        character,
        class,
        concatenation,
        group,
        quantifier,
        wildcard,
    };
};
