const std = @import("std");
const mem = std.mem;
const Glushkov = @import("Glushkov.zig");
const EqClass = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Bitset256 = std.bit_set.IntegerBitSet(256);
const HashMap = std.AutoArrayHashMapUnmanaged;

class: [256]u16 = undefined,
count: u16 = 0,

pub fn init() EqClass {
    return .{
        .class = &[_]u16{},
        .count = 0,
    };
}

pub fn compute(self: *EqClass, allocator: mem.Allocator, glushkov: *Glushkov, root: *Ast.Node) !void {
    try glushkov.glushkovFromAst(root);
    const N = glushkov.symbols.len;

    var sig: [256]Bitset256 = undefined;
    for (0..256) |c| {
        var bs = Bitset256.initEmpty();
        for (glushkov.symbols.buffer[0..N], 0..) |sym, i| {
            if (sym.matches(@as(u8, c))) bs.set(i);
        }
        sig[c] = bs;
    }

    var cmaps = HashMap(Bitset256, u16).empty;
    var next_class: u16 = 0;
    @memset(self.class, 0);

    for (0..256) |c| {
        const bs = sig[c];

        if (cmaps.get(bs)) |index| {
            self.class[c] = index;
        } else {
            self.class[c] = next_class;
            try cmaps.put(allocator, bs, next_class);
            next_class += 1;
        }
    }
    self.count = next_class;
}
