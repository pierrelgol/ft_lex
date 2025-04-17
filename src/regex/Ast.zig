const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const assert = std.assert;

pool: heap.MemoryPool(Node),
root: ?*Node,

pub const Node = union(enum) {};
