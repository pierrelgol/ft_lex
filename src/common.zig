const std = @import("std");

pub fn Iterator(comptime T: type) type {
    return struct {
        const Self = @This();

        items: []const T = undefined,
        index: usize = 0,
        saved: usize = 0,

        pub fn init(items: []const T) Self {
            return .{
                .items = items,
                .index = 0,
                .saved = 0,
            };
        }

        pub fn advance(self: *Self) void {
            self.index += 1;
        }

        pub fn next(self: *Self) ?T {
            if (self.index >= self.items.len) return null;
            defer self.index += 1;
            return self.items[self.index];
        }

        pub fn peek(self: *Self) ?T {
            if (self.index + 1 >= self.items.len) return null;
            return self.items[self.index + 1];
        }

        pub fn save(self: *Self) void {
            self.saved = self.index;
        }

        pub fn restore(self: *Self) void {
            self.index = self.saved;
        }
    };
}
