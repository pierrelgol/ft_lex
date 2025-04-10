const std = @import("std");

pub const Atom = union(Kind) {
    alternation: Alternation,
    anchor: Anchor,
    any: Any,
    class_begin: ClassBegin,
    class_end: ClassEnd,
    eof: Eof,
    group_begin: GroupBegin,
    group_end: GroupEnd,
    identifier: Identifier,
    literal: Literal,
    number: Number,
    posix_class: PosixClass,
    quantifier: Quantifier,
    range: Range,

    pub fn init(comptime kind: Kind, value: anytype) Atom {
        return @unionInit(Atom, @tagName(kind), value);
    }
};

pub const Kind = enum {
    alternation,
    anchor,
    any,
    class_begin,
    class_end,
    eof,
    group_begin,
    group_end,
    identifier,
    literal,
    number,
    posix_class,
    quantifier,
    range,
};

pub const Alternation = {};

pub const Any = {};

pub const Anchor = enum { start, end };

pub const ClassBegin = {};

pub const ClassEnd = {};

pub const Eof = {};

pub const GroupBegin = {};

pub const GroupEnd = {};

pub const Identifier = struct { value: u8 };

pub const Literal = struct { value: u8 };

pub const Number = struct { value: u8 };

pub const PosixClass = enum {
    alnum,
    alpha,
    ascii,
    blank,
    cntrl,
    digit,
    graph,
    lower,
    print,
    punct,
    space,
    upper,
    word,
    xdigit,
};
pub const Quantifier = struct {
    min: usize,
    max: ?usize,
    greedy: bool,

    pub const zero_or_more: Quantifier = .{
        .min = 0,
        .max = null,
        .greedy = false,
    };

    pub const one_or_more: Quantifier = .{
        .min = 1,
        .max = null,
        .greedy = false,
    };

    pub const zero_or_one: Quantifier = .{
        .min = 0,
        .max = 1,
        .greedy = true,
    };
};
pub const Range = struct { begin: u8, end: u8 };
