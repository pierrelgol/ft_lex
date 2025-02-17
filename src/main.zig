const std = @import("std");
const Parser = @import("Parser.zig");
const test_input = @import("lexinputs").c99_identifiers;

pub fn main() !void {
    var base_allocator = std.heap.DebugAllocator(.{}){};
    defer _ = base_allocator.deinit();

    const allocator = base_allocator.allocator();

    var parser: Parser = .init(allocator);
    defer parser.deinit();

    try parser.parse("c99_identifiers.l", test_input);
}
