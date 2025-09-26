const std = @import("std");

pub const TokenKind = enum {
    Identifier,
    LiteralNumber,

    OperatorPlus,
    OperatorMinus,
    OperatorStar,
    OperatorSlash,

    SymbolColon,
    SymbolSemiColon,
    SymbolEqual,
    SymbolOpenParen,
    SymbolCloseParen,
    SymbolComma,

    KeywordMut,
    KeywordU8,
    KeywordU16,
    KeywordU32,
    KeywordU64,
    KeywordI8,
    KeywordI16,
    KeywordI32,
    KeywordI64,
    KeywordF32,
    KeywordF64,
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

    pub fn pushToken(self: *Lexer, kind: TokenKind, value: []const u8) void {
        self.tokens.append(self.allocator, Token.init(kind, value, self)) catch unreachable;
    }

    pub fn consumeWhitespace(self: *Lexer) void {
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
            '+' => .OperatorPlus,
            '-' => .OperatorMinus,
            '*' => .OperatorStar,
            '/' => .OperatorSlash,
            '(' => .SymbolOpenParen,
            ')' => .SymbolCloseParen,
            ':' => .SymbolColon,
            ';' => .SymbolSemiColon,
            '=' => .SymbolEqual,
            ',' => .SymbolComma,
            else => {
                std.debug.print("invalid symbol: {c}\n", .{char});
                return LexerError.InvalidSymbol;
            },
        };

        self.pushToken(kind, self.source[self.cursor - 1 .. self.cursor]);
    }

    pub fn getKeyword(name: []const u8) ?TokenKind {
        // zig cannot switch on strings

        if (std.mem.eql(u8, name, "mut")) return .KeywordMut;
        if (std.mem.eql(u8, name, "u8")) return .KeywordU8;
        if (std.mem.eql(u8, name, "u16")) return .KeywordU16;
        if (std.mem.eql(u8, name, "u32")) return .KeywordU32;
        if (std.mem.eql(u8, name, "u64")) return .KeywordU64;
        if (std.mem.eql(u8, name, "i8")) return .KeywordI8;
        if (std.mem.eql(u8, name, "i16")) return .KeywordI16;
        if (std.mem.eql(u8, name, "i32")) return .KeywordI32;
        if (std.mem.eql(u8, name, "i64")) return .KeywordI64;
        if (std.mem.eql(u8, name, "f32")) return .KeywordF32;
        if (std.mem.eql(u8, name, "f64")) return .KeywordF64;

        return null;
    }

    pub fn consumeIdentifier(self: *Lexer) !void {
        const start_idx = self.cursor;
        var end_idx = start_idx;

        while (self.ok() and std.ascii.isAlphanumeric(self.peek())) {
            end_idx += 1;
            _ = self.consume();
        }

        const name = self.source[start_idx..end_idx];
        self.pushToken(Lexer.getKeyword(name) orelse .Identifier, name);
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

        self.pushToken(.LiteralNumber, self.source[start_idx..end_idx]);
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
