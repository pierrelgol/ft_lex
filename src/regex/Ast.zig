const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const fmt = std.fmt;
const Allocator = mem.Allocator;
const String = std.ArrayList(u8); // Usually not needed for formatting directly

const Charset = std.StaticBitSet(std.math.maxInt(u8));

pub const Ast = struct {
    pool: heap.MemoryPool(Node),
    root: ?*Node,

    pub const Node = union(Kind) {
        literal: u8,
        // concatenation: struct { lhs: *Node, rhs: *Node },
        // alternation: struct { lhs: *Node, rhs: *Node },
        // quantifier: struct { min: u32, max: ?u32, greedy: bool, target: *Node },
        // class: struct { negated: bool, charset: Charset },
        // anchor_start: struct{},
        // anchor_end: struct{},
        // group: *Node,
        // quoted_string: []const u8, // Needs allocator awareness if stored directly
        // definition_ref: []const u8, // Needs allocator awareness if stored directly

        pub const Kind = enum {
            literal,
            // FUTURE KINDS:
            // concatenation,
            // alternation,
            // quantifier,
            // class,
            // anchor_start,
            // anchor_end,
            // group,
            // quoted_string,
            // definition_ref,
        };

        pub fn tag(self: *const Node) Kind {
            return std.meta.activeTag(self.*);
        }

        pub fn format(
            self: @This(),
            comptime format_string: []const u8,
            options: fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = format_string;
            _ = options;

            switch (self) {
                .literal => |char| {
                    // Handle printable vs non-printable, escaping '(', ')', ' ', '\' if needed
                    if (fmt.isPrintableAscii(char) and char != '(' and char != ')' and char != '\\') {
                        try writer.print("(literal {c})", .{char});
                    } else {
                        // Use standard escape sequences
                        try writer.print("(literal '\\{any}')", .{fmt.fmtEscapedChar(char)});
                    }
                },

                // Add cases for other node kinds as they are implemented
                // Example:
                // .concatenation => |payload| {
                //    // writer.print calls payload.lhs.format(...) recursively
                //    try writer.print("(concatenation {} {})", .{ payload.lhs, payload.rhs });
                // },
                // .alternation => |payload| {
                //     try writer.print("(alternation {} {})", .{ payload.lhs, payload.rhs });
                // },
                // .quantifier => |payload| {
                //     // Need to format max=?null case nicely
                //     const max_str = if (payload.max) |m| fmt.comptimePrint("{}", .{m}) else "inf";
                //     try writer.print("(quantifier min={} max={} greedy={} {})", .{ payload.min, max_str, payload.greedy, payload.target });
                // },
                // .class => |payload| {
                //     // Need a helper to format the Charset, e.g., to "[abc-f]"
                //     var charset_buffer: [100]u8 = undefined; // Adjust size
                //     const charset_str = try formatCharset(&charset_buffer, payload.charset);
                //     try writer.print("(class negated={} {s})", .{ payload.negated, charset_str });
                // },
                // .group => |payload| {
                //    try writer.print("(group {})", .{payload});
                // },
                // .anchor_start => {
                //    try writer.writeAll("(anchor ^)");
                // },
                // .anchor_end => {
                //    try writer.writeAll("(anchor $)");
                // },

                // Add default or else => unreachable if all kinds should be handled
                // else => @panic("Unknown AST node kind"),
            }
        }
    };

    pub fn init(gpa: Allocator) Ast {
        return .{
            .pool = heap.MemoryPool(Node).init(gpa),
            .root = null,
        };
    }

    pub fn deinit(self: *Ast) void {
        defer self.* = undefined;
        self.pool.deinit();
    }

    pub fn createNode(self: *Ast, comptime kind: Node.Kind, init_value: anytype) Allocator.Error!*Node {
        const node_ptr = try self.pool.create();
        node_ptr.* = @unionInit(Node, @tagName(kind), init_value);
        return node_ptr;
    }

    pub fn format(
        self: @This(),
        comptime fmt_string: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_string;
        _ = options;

        if (self.root) |root_node| {
            try writer.print("{}", .{root_node.*});
        } else {
            try writer.writeAll("(empty ast)");
        }
    }
};
