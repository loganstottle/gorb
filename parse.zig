const std = @import("std");
const lex = @import("lex.zig");

const Token = lex.Token;
const TokenKind = lex.TokenKind;

pub const ParseExprError = error{
    OutOfMemory,
    InvalidCharacter,
};

pub const BinaryOperator = enum {
    Add,
    Subtract,
    Multiply,
    Divide,

    pub fn precedence(self: BinaryOperator) u32 {
        if (self == BinaryOperator.Add) return 2;
        if (self == BinaryOperator.Subtract) return 2;
        if (self == BinaryOperator.Multiply) return 1;
        if (self == BinaryOperator.Divide) return 1;

        unreachable;
    }

    pub fn associativity(_: BinaryOperator) u32 {
        return 0;
    }
};

pub const Expression = union(enum) {
    number_literal: f64,
    identifier: []const u8,
    fn_call: struct {
        name: []const u8,
        args: std.ArrayList(*Expression) = .empty,
    },
    binary: struct {
        operator: BinaryOperator,
        left: *Expression,
        right: *Expression,
    },

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |e| {
                e.left.deinit(allocator);
                e.right.deinit(allocator);
            },
            .fn_call => |f| {
                for (f.args.items) |expr| expr.deinit(allocator);
                @constCast(&f.args).deinit(allocator);
            },
            else => {},
        }

        allocator.destroy(self);
    }
};

pub const Statement = union(enum) {
    var_declare: struct { name: []const u8, value: *Expression },
    var_assign: struct { name: []const u8, value: *Expression },
    expression: *Expression,

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        switch (self) {
            .var_declare => |s| s.value.deinit(allocator),
            .var_assign => |s| s.value.deinit(allocator),
            .expression => |s| s.deinit(allocator),
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    cursor: u32 = 0,
    program: std.ArrayList(Statement) = .empty,

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return .{ .allocator = allocator, .tokens = tokens };
    }

    pub fn deinit(self: *Parser) void {
        for (self.program.items) |statement| statement.deinit(self.allocator);
        self.program.deinit(self.allocator);
    }

    pub fn ok(self: *Parser) bool {
        return self.cursor < self.tokens.len;
    }

    pub fn peek(self: *Parser) Token {
        return self.tokens[self.cursor];
    }

    pub fn consume(self: *Parser) Token {
        const tok = self.tokens[self.cursor];
        self.cursor += 1;
        return tok;
    }

    pub fn expect(self: *Parser, kind: TokenKind) void {
        if (!self.ok()) {
            std.debug.print("expected {any}, found EOF\n", .{kind});
            std.process.exit(0);
        }

        const found_kind = self.consume().kind;

        if (found_kind != kind) {
            std.debug.print("expected {any}, found {any}\n", .{ kind, found_kind });
            std.process.exit(0);
        }
    }

    pub fn convertBinaryOperator(_: *Parser, token: Token) ?BinaryOperator {
        return switch (token.kind) {
            .Plus => .Add,
            .Minus => .Subtract,
            .Star => .Multiply,
            .Slash => .Divide,
            else => null,
        };
    }

    pub fn parseFnCall(self: *Parser) !*Expression {
        const name = self.consume().value;

        self.expect(.OpenParen);

        const expr = try self.allocator.create(Expression);
        expr.* = .{ .fn_call = .{ .name = name } };

        while (self.peek().kind != .CloseParen) {
            try expr.*.fn_call.args.append(self.allocator, try self.parseExpression());
            if (self.peek().kind != .CloseParen) self.expect(.Comma);
        }

        self.expect(.CloseParen);

        std.debug.print("FN CALL: {any}\n", .{expr.*.fn_call.args.items});

        return expr;
    }

    pub fn parseAtom(self: *Parser) !*Expression {
        switch (self.peek().kind) {
            .Identifier => {
                if (self.cursor + 1 < self.tokens.len and self.tokens[self.cursor + 1].kind == .OpenParen)
                    return self.parseFnCall();

                const result = try self.allocator.create(Expression);
                result.* = .{ .identifier = self.consume().value };

                return result;
            },
            .NumberLiteral => {
                const result = try self.allocator.create(Expression);
                result.* = .{ .number_literal = try std.fmt.parseFloat(f64, self.consume().value) };

                return result;
            },
            .OpenParen => {
                _ = self.consume();
                const expr = try self.parseExpression();
                self.expect(.CloseParen);

                return expr;
            },
            else => {
                std.debug.print("expected expression: {any}\n", .{self.peek().kind});
                std.process.exit(0);
            },
        }
    }

    fn parseExpressionPrec(self: *Parser, min_prec: u32) !*Expression {
        var lhs = try self.parseAtom();

        while (self.ok()) {
            const op = self.convertBinaryOperator(self.peek()) orelse break;

            if (op.precedence() > min_prec)
                break;

            var next_min_prec = op.precedence();
            if (op.associativity() == 0) next_min_prec -= 1;

            _ = self.consume();

            const next_lhs = try self.allocator.create(Expression);
            next_lhs.* = .{
                .binary = .{
                    .operator = op,
                    .left = lhs,
                    .right = try self.parseExpressionPrec(next_min_prec),
                },
            };

            lhs = next_lhs;
        }

        return lhs;
    }

    pub fn parseExpression(self: *Parser) ParseExprError!*Expression {
        return try self.parseExpressionPrec(std.math.maxInt(u32));
    }

    pub fn parseVarDeclare(self: *Parser) !void {
        const ident = self.consume();
        self.expect(.Colon);
        self.expect(.Equal);

        try self.program.append(self.allocator, .{ .var_declare = .{ .name = ident.value, .value = try self.parseExpression() } });
    }

    pub fn parseVarAssign(self: *Parser) !void {
        const ident = self.consume();
        self.expect(.Equal);

        try self.program.append(self.allocator, .{ .var_assign = .{ .name = ident.value, .value = try self.parseExpression() } });
    }

    pub fn parse(self: *Parser) !void {
        while (self.ok()) {
            switch (self.peek().kind) {
                .Identifier => {
                    if (self.cursor + 1 < self.tokens.len) {
                        switch (self.tokens[self.cursor + 1].kind) {
                            .Colon => try self.parseVarDeclare(),
                            .Equal => try self.parseVarAssign(),
                            else => try self.program.append(self.allocator, .{ .expression = try self.parseExpression() }),
                        }
                    } else try self.program.append(self.allocator, .{ .expression = try self.parseExpression() });
                },
                else => try self.program.append(self.allocator, .{ .expression = try self.parseExpression() }),
            }

            self.expect(.SemiColon);
        }
    }
};
