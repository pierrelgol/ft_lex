const std = @import("std");
const io = std.io;
const proc = std.process;
const fs = std.fs;
const mem = std.mem;
const heap = std.heap;
const log = std.log;
const Parser = @import("Parser.zig");
const Glushkov = @import("Glushkov.zig").Glushkov;
const stdout = io.getStdOut().writer();

pub const LexArgs = struct {
    flag_t: bool = false,
    flag_p: bool = false,
    flag_n: bool = false,
    flag_use_stdin: bool = false,
    file_path: ?[:0]const u8 = null,
};

var flags: LexArgs = .{};

fn printUsage() void {
    stdout.print("./ft_lex <file_path>\n", .{}) catch {};
}

fn fatal(err: anyerror) noreturn {
    log.err("Fatal error : {!}", .{err});
    proc.exit(((@as(u8, @truncate(@intFromError(err))))));
}

fn openFileOrStdin(maybe_file_path: ?[:0]const u8) !fs.File {
    const file_path = maybe_file_path orelse return io.getStdIn();
    return try fs.cwd().openFile(file_path, .{ .mode = .read_only });
}

// pub fn main() !u8 {
//     var gpa_instance = heap.DebugAllocator(.{}).init;
//     defer _ = gpa_instance.deinit();

//     const gpa = gpa_instance.allocator();

//     var arguments = proc.argsWithAllocator(gpa) catch |err| {
//         fatal(err);
//     };
//     defer arguments.deinit();

//     if (!arguments.skip()) {
//         printUsage() catch |err| fatal(err);
//     }

//     while (arguments.next()) |arg| {
//         if (mem.startsWith(u8, arg, "-")) {
//             for (arg) |c| {
//                 switch (c) {
//                     't' => flags.flag_t = true,
//                     'n' => flags.flag_n = true,
//                     'p' => flags.flag_p = true,
//                     else => continue,
//                 }
//             }
//         } else {
//             flags.file_path = arg;
//         }
//     }

//     var input_file: fs.File = openFileOrStdin(flags.file_path) catch |err| {
//         fatal(err);
//     };
//     defer input_file.close();

//     // @TODO(handle stdin interractive with .istty);
//     var input_buffer = input_file.readToEndAlloc(gpa, std.math.maxInt(i32)) catch |err| {
//         fatal(err);
//     };
//     defer gpa.free(input_buffer);

//     std.debug.print("{s}", .{input_buffer});
//     _ = &input_buffer;

//     return 0;
// }

pub fn main() !void {
    var buffer: [32 * std.heap.pageSize()]u8 = undefined;
    var fba_instance = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const fba = fba_instance.allocator();

    const pattern = "(0[xX][0-9a-fA-F]+|[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?)[uUlL]*";
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
