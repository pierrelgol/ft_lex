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

pub const LocationIndex = usize;
