const std = @import("std");
const Parser = @import("regex.zig").Parser;

pub fn foo() void {}
pub fn main() !void {
    var buffer: [8 * std.heap.pageSize()]u8 = undefined;
    var fba_instance = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const fba = fba_instance.allocator();

    for (0..1_000_000) |_| {
        defer fba_instance.reset();
        var parser: Parser = .init(fba, "([0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?)|0[xX][0-9A-Fa-f]+|0[bB][01]+|0[0-7]+|[A-Za-z_][A-Za-z0-9_]*|\"([^\"\\]|\\.)*\"|'([^'\\]|\\.)*' ");
        defer parser.deinit();
        _ = try parser.parse();
    }
}
