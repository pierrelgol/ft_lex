const std = @import("std");

pub const TokenResult = struct {
    token: Token,
    bytes: usize,

    pub const EOF: TokenResult = .{ .token = .{ .end_of_expression = {} }, .bytes = 0 };
};

pub const Token = union(Kind) {
    literal: u21,
    start_anchor: void,
    end_anchor: void,
    any_character: void,
    alternation: void,
    zero_or_more: void,
    one_or_more: void,
    zero_or_one: void,
    open_group: void,
    close_group: void,
    open_bracket: void,
    close_bracket: void,
    open_interval_brace: void,
    close_interval_brace: void,
    interval_comma: void,
    interval_number: u32,
    backslash: void,
    backreference: u8,
    bracket_range_operator: void,
    bracket_negation: void,
    character_class_start: void,
    character_class_end: void,
    character_class_name: []const u8,
    collating_element_start: void,
    collating_element_end: void,
    collating_element_name: []const u8,
    equivalence_class_start: void,
    equivalence_class_end: void,
    equivalence_class_name: []const u8,
    end_of_expression: void,

    pub const Kind = enum {
        literal,
        start_anchor,
        end_anchor,
        any_character,
        alternation,
        zero_or_more,
        one_or_more,
        zero_or_one,
        open_group,
        close_group,
        open_bracket,
        close_bracket,
        open_interval_brace,
        close_interval_brace,
        interval_comma,
        interval_number,
        backslash,
        backreference,
        bracket_range_operator,
        bracket_negation,
        character_class_start,
        character_class_end,
        character_class_name,
        collating_element_start,
        collating_element_end,
        collating_element_name,
        equivalence_class_start,
        equivalence_class_end,
        equivalence_class_name,
        end_of_expression,
    };

    pub fn tag(self: *const Token) Token.Kind {
        return std.meta.activeTag(self.*);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .start_anchor => try writer.print("^", .{}),
            .end_anchor => try writer.print("$", .{}),
            .any_character => try writer.print(".", .{}),
            .alternation => try writer.print("|", .{}),
            .zero_or_more => try writer.print("*", .{}),
            .one_or_more => try writer.print("+", .{}),
            .zero_or_one => try writer.print("?", .{}),
            .open_group => try writer.print("(", .{}),
            .close_group => try writer.print(")", .{}),
            .open_bracket => try writer.print("[", .{}),
            .close_bracket => try writer.print("]", .{}),
            .open_interval_brace => try writer.print("{{", .{}),
            .close_interval_brace => try writer.print("}}", .{}),
            .interval_comma => try writer.print(",", .{}),
            .backslash => try writer.print("\\", .{}),
            .bracket_range_operator => try writer.print("-", .{}),
            .bracket_negation => try writer.print("^", .{}),
            .character_class_start => try writer.print("[:", .{}),
            .character_class_end => try writer.print(":]", .{}),
            .collating_element_start => try writer.print("[.", .{}),
            .collating_element_end => try writer.print(".]", .{}),
            .equivalence_class_start => try writer.print("[=", .{}),
            .equivalence_class_end => try writer.print("=]", .{}),
            .literal => |v| try writer.print("{u}", .{v}),
            .interval_number => |v| try writer.print("{d}", .{v}),
            .backreference => |v| try writer.print("\\{d}", .{v}),
            .character_class_name => |v| try writer.print("{s}", .{v}),
            .collating_element_name => |v| try writer.print("{s}", .{v}),
            .equivalence_class_name => |v| try writer.print("{s}", .{v}),
            .end_of_expression => try writer.print("EOF", .{}),
        }
    }
};
