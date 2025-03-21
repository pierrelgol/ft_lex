// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Token.zig                                          :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 17:56:05 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 17:56:05 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const mem = std.mem;
const SourceLocation = @import("SourceLocation.zig");
const Token = @This();

location: SourceLocation = .{},
text: []const u8 = "",

pub fn init(location: SourceLocation, text: ?[]const u8) Token {
    return .{
        .location = location,
        .text = text orelse "",
    };
}

pub fn format(
    self: @This(),
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try writer.print("{}[{s}]", .{ self.location, self.text });
}
