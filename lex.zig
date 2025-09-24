const std = @import("std");

pub const TokenKind = enum {
    Identifier,
    NumberLiteral,

    Plus,
    Minus,
    Star,
    Slash,
    Colon,
    SemiColon,
    Equal,
    OpenParen,
    CloseParen,
    Comma,
};

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
        self.col += 1;
        return result;
    }

    pub fn pushToken(self: *Lexer, kind: TokenKind, value: []const u8) !void {
        try self.tokens.append(self.allocator, Token.init(kind, value, self));
    }

    pub fn consumeWhitespace(self: *Lexer) !void {
        switch (self.consume()) {
            '\n' => {
                self.line += 1;
                self.col = 0;
            },
            ' ', '\r', '\t', '\x0B', '\x0C' => self.col += 1,
            else => unreachable,
        }
    }

    pub fn consumeSymbol(self: *Lexer) !void {
        const char = self.consume();

        const kind: TokenKind = switch (char) {
            '+' => .Plus,
            '-' => .Minus,
            '*' => .Star,
            '/' => .Slash,
            '(' => .OpenParen,
            ')' => .CloseParen,
            ':' => .Colon,
            ';' => .SemiColon,
            '=' => .Equal,
            ',' => .Comma,
            else => {
                std.debug.print("invalid symbol: {c}\n", .{char});
                return LexerError.InvalidSymbol;
            },
        };

        try self.pushToken(kind, self.source[self.cursor - 1 .. self.cursor]);
    }

    pub fn consumeIdentifier(self: *Lexer) !void {
        const start_idx = self.cursor;
        var end_idx = start_idx;

        while (self.ok() and std.ascii.isAlphanumeric(self.peek())) {
            end_idx += 1;
            _ = self.consume();
        }

        try self.pushToken(.Identifier, self.source[start_idx..end_idx]);
    }

    pub fn consumeNumber(self: *Lexer) !void {
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

        try self.pushToken(.NumberLiteral, self.source[start_idx..end_idx]);
    }

    pub fn tokenize(self: *Lexer) !void {
        while (self.ok()) {
            try switch (self.peek()) {
                ' ', '\n', '\r', '\t', '\x0B', '\x0C' => self.consumeWhitespace(),
                '0'...'9' => self.consumeNumber(),
                '_', 'a'...'z', 'A'...'Z' => self.consumeIdentifier(),
                else => self.consumeSymbol(),
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
