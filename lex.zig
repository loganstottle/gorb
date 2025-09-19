const std = @import("std");

pub const TokenKind = enum { NumberLiteral, Identifier, Plus, Minus, Star, Slash, OpenParen, CloseParen };

pub const Token = struct {
    kind: TokenKind,
    value: []const u8,
    line: u32,
    col: u32,
    filename: []const u8,

    pub fn init(kind: TokenKind, value: []const u8, lexer: *Lexer) Token {
        return .{ .kind = kind, .value = value, .line = lexer.line, .col = lexer.col, .filename = lexer.filename };
    }
};

pub const LexerError = error{ InvalidSymbol, TooManyPeriods };

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32 = 0,
    col: u32 = 0,
    cursor: u32 = 0,
    tokens: std.ArrayList(Token) = .empty,
    filename: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, filename: []const u8) Lexer {
        return .{ .allocator = allocator, .source = source, .filename = filename };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit(self.allocator);
    }

    pub fn ok(self: *Lexer) bool {
        return self.cursor < self.source.len;
    }

    pub fn peek(self: *Lexer) u8 {
        return self.source[self.cursor];
    }

    pub fn consume(self: *Lexer) u8 {
        const result = self.source[self.cursor];
        self.cursor += 1;
        return result;
    }

    pub fn push_token(self: *Lexer, kind: TokenKind, value: []const u8) !void {
        try self.tokens.append(self.allocator, Token.init(kind, value, self));
    }

    pub fn consume_whitespace(self: *Lexer) !void {
        switch (self.consume()) {
            '\n' => {
                self.line += 1;
                self.col = 0;
            },
            ' ', '\r', '\t', '\x0B', '\x0C' => self.col += 1,
            else => unreachable,
        }
    }

    pub fn consume_symbol(self: *Lexer) !void {
        const char = self.consume();

        const kind: TokenKind = switch (char) {
            '+' => .Plus,
            '-' => .Minus,
            '*' => .Star,
            '/' => .Slash,
            '(' => .OpenParen,
            ')' => .CloseParen,
            else => {
                std.debug.print("invalid symbol: {c}\n", .{char});
                return LexerError.InvalidSymbol;
            },
        };

        try self.push_token(kind, self.source[self.cursor - 1 .. self.cursor]);
    }

    pub fn consume_identifier(self: *Lexer) !void {
        const start_idx = self.cursor;
        var end_idx = start_idx;

        while (self.ok() and std.ascii.isAlphanumeric(self.consume()))
            end_idx += 1;

        try self.push_token(.Identifier, self.source[start_idx..end_idx]);
    }

    pub fn consume_number(self: *Lexer) !void {
        const start_idx = self.cursor;
        var end_idx = start_idx;

        var seen_period = false;

        while (self.ok() and (std.ascii.isDigit(self.peek()) or self.peek() == '.')) {
            if (self.consume() == '.') {
                if (seen_period) return LexerError.TooManyPeriods;
                seen_period = true;
            }

            end_idx += 1;
        }

        try self.push_token(.NumberLiteral, self.source[start_idx..end_idx]);
    }

    pub fn tokenize(self: *Lexer) !void {
        while (self.ok()) {
            try switch (self.peek()) {
                ' ', '\n', '\r', '\t', '\x0B', '\x0C' => self.consume_whitespace(),
                '0'...'9' => self.consume_number(),
                '_', 'a'...'z', 'A'...'Z' => self.consume_identifier(),
                else => self.consume_symbol(),
            };
        }
    }

    pub fn log(self: *Lexer) void {
        std.debug.print("Tokens:\n", .{});

        for (self.tokens.items) |tok| {
            std.debug.print("  {{ {any} \"{s}\" }}\n", .{ tok.kind, tok.value });
        }
    }
};
