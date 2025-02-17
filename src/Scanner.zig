// ************************************************************************** //
//                                                                            //
//                                                        :::      ::::::::   //
//   Scanner.zig                                        :+:      :+:    :+:   //
//                                                    +:+ +:+         +:+     //
//   By: pollivie <pollivie.student.42.fr>          +#+  +:+       +#+        //
//                                                +#+#+#+#+#+   +#+           //
//   Created: 2025/02/17 11:04:23 by pollivie          #+#    #+#             //
//   Updated: 2025/02/17 11:04:24 by pollivie         ###   ########.fr       //
//                                                                            //
// ************************************************************************** //

const std = @import("std");
const io = std.io;
const mem = std.mem;
const SourceLocation = @import("SourceLocation.zig");
const LocationIndex = SourceLocation.LocationIndex;
const ArrayList = std.ArrayList;
const Scanner = @This();

buffer: []const u8,
index: usize,
saved: usize,

pub fn init(buffer: []const u8) Scanner {
    return .{
        .buffer = buffer,
        .index = 0,
        .saved = 0,
    };
}

pub fn next(self: *Scanner) ?u8 {
    if (self.eof()) {
        return null;
    }
    defer self.index += 1;
    return self.buffer[self.index];
}

pub fn prev(self: *Scanner) ?u8 {
    if (self.index == 0) {
        return null;
    }
    self.index -= 1;
    return self.buffer[self.index];
}

pub fn curr(self: *Scanner) ?u8 {
    if (self.eof()) {
        return null;
    }
    return self.buffer[self.index];
}

pub fn peek(self: *Scanner) ?u8 {
    if (self.index + 1 >= self.buffer.len) {
        return null;
    }
    return self.buffer[self.index + 1];
}

pub fn advance(self: *Scanner) void {
    if (self.eof()) {
        return;
    }
    self.index += 1;
}

pub fn eof(self: *const Scanner) bool {
    return self.index >= self.buffer.len;
}

pub fn skip(self: *Scanner, matchFn: *const fn (u8) bool) usize {
    var count: usize = 0;
    while (!self.eof()) {
        const char = self.curr() orelse return count;
        if (matchFn(char)) {
            count += 1;
            self.advance();
        } else {
            return count;
        }
    }
    return count;
}

pub fn match(self: *Scanner, matchFn: *const fn (u8) bool) bool {
    self.save();
    defer self.restore();

    while (!self.eof()) {
        const char = self.curr() orelse return false;
        if (matchFn(char)) {
            return true;
        } else {
            self.advance();
        }
    }

    return false;
}

pub fn save(self: *Scanner) void {
    self.saved = self.index;
}

pub fn restore(self: *Scanner) void {
    self.index = self.saved;
}

pub fn reset(self: *Scanner) void {
    self.index = 0;
    self.saved = 0;
}

pub const CountingScanner = struct {
    scanner: *Scanner,
    delimiter: u8,
    count: usize,
    saved: usize,

    pub fn init(scanner: *Scanner, delimiter: u8) CountingScanner {
        return .{
            .scanner = scanner,
            .delimiter = delimiter,
            .count = 0,
        };
    }

    pub fn next(self: *CountingScanner) ?u8 {
        return if (self.scanner.next()) |char| {
            if (char == self.delimiter) {
                self.count += 1;
            }
            return char;
        } else null;
    }

    pub fn prev(self: *CountingScanner) ?u8 {
        return if (self.scanner.prev()) |char| {
            if (char == self.delimiter) {
                self.count -|= 1;
            }
            return char;
        } else null;
    }

    pub fn curr(self: *CountingScanner) ?u8 {
        return self.scanner.curr();
    }

    pub fn peek(self: *CountingScanner) ?u8 {
        return self.scanner.peek();
    }

    pub fn eof(self: *const CountingScanner) bool {
        return self.scanner.eof();
    }

    pub fn skip(self: *CountingScanner, matchFn: *const fn (u8) bool) usize {
        var count: usize = 0;
        while (!self.eof()) {
            const char = self.curr() orelse return count;
            if (char == self.delimiter) {
                self.count += 1;
            }
            if (matchFn(char)) {
                count += 1;
                self.advance();
            } else {
                return count;
            }
        }
        return count;
    }

    pub fn match(self: *CountingScanner, matchFn: *const fn (u8) bool) bool {
        self.save();
        defer self.restore();

        while (!self.eof()) {
            const char = self.curr() orelse return false;

            if (char == self.delimiter) {
                self.count += 1;
            }

            if (matchFn(char)) {
                return true;
            } else {
                self.advance();
            }
        }

        return false;
    }

    pub fn advance(self: *CountingScanner) bool {
        if (self.scanner.index >= self.scanner.buffer.len) {
            return false;
        }
        self.scanner.index += 1;
        return true;
    }

    pub fn reset(self: *CountingScanner) void {
        self.count = 0;
        self.saved = 0;
        self.scanner.reset();
    }

    pub fn save(self: *CountingScanner) void {
        self.scanner.save();
        self.saved = self.count;
    }

    pub fn restore(self: *CountingScanner) void {
        self.scanner.restore();
        self.count = self.saved;
    }

    pub fn getCount(self: *CountingScanner) usize {
        return self.count;
    }

    pub fn clearCount(self: *CountingScanner) void {
        self.count = 0;
    }

    pub fn changeDelimiter(self: *CountingScanner, delimiter: u8) void {
        self.delimiter = delimiter;
        self.count = 0;
    }
};
