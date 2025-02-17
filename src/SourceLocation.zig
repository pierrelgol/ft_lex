// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   SourceLocation.zig                                 :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 11:07:58 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 11:07:59 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const SourceLocation = @This();

filename: []const u8 = "",
line: usize = 0,
column: usize = 0,

pub fn init(filename: []const u8, line: usize, column: usize) SourceLocation {
    return .{
        .filename = filename,
        .line = line,
        .column = column,
    };
}

pub fn nextLine(self: *SourceLocation) void {
    self.line += 1;
    self.column = 0;
}

pub fn moveCol(self: *SourceLocation, amount: usize) void {
    self.column += amount;
}

pub fn format(
    self: @This(),
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try writer.print("{s} line:{d}:col:{d}\n", .{ self.filename, self.line, self.column });
}

pub const LocationIndex = usize;
