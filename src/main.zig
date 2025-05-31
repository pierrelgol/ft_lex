const std = @import("std");
const Parser = @import("Parser.zig");
const Glushkov = @import("Glushkov.zig").Glushkov;

pub fn main() !void {
    var buffer: [4 * std.heap.pageSize()]u8 = undefined;
    var fba_instance = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const fba = fba_instance.allocator();

    const pattern = "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]).){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]) ";
    const N = 1000;

    var total_ns: i128 = 0;
    for (0..N) |_| {
        var parser: Parser = .init(fba, pattern);
        defer parser.deinit();
        var glushkov = Glushkov.init(fba);
        defer glushkov.deinit();

        const start = std.time.nanoTimestamp();
        const ast = try parser.parse();
        try glushkov.glushkovFromAst(ast.root.?);
        const end = std.time.nanoTimestamp();
        total_ns += end - start;
        fba_instance.reset();
    }
    std.debug.print("Avg parse time: {}\n", .{std.fmt.fmtDurationSigned(@truncate(@divFloor(total_ns, N)))});
}
