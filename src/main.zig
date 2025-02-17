const std = @import("std");
const Parser = @import("Parser.zig");
const Scanner = @import("Scanner.zig");
const test_input = @import("lexinputs").c99_identifiers;

pub fn main() !void {
    var base_allocator = std.heap.DebugAllocator(.{}){};
    defer _ = base_allocator.deinit();

    const allocator = base_allocator.allocator();

    const scanner: Scanner = .init(test_input);

    var parser: Parser = .init(allocator, scanner);
    defer parser.deinit();

    try parser.parse();
}
