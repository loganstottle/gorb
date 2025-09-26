const std = @import("std");
const lex = @import("lex.zig");

const Token = lex.Token;
const TokenKind = lex.TokenKind;

pub const TypeKind = union(enum) {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Boolean,
    Pointer: *TypeKind,
    Auto,
    Void,
};

pub const Type = struct {
    kind: TypeKind,
    mutable: bool,
};

pub const BinaryOperator = enum {
    Add,
    Subtract,
    Multiply,
    Divide,

    pub fn precedence(self: BinaryOperator) u8 {
        return switch (self) {
            .Add, .Subtract => 2,
            .Multiply, .Divide => 1,
        };
    }

    pub fn associativity(_: BinaryOperator) u8 {
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
            .fn_call => |*f| {
                for (f.args.items) |expr| expr.deinit(allocator);
                f.args.deinit(allocator);
            },
            else => {},
        }

        allocator.destroy(self);
    }
};

pub const VarDeclare = struct {
    type: Type,
    name: []const u8,
    value: *Expression,
};

pub const VarAssign = struct { name: []const u8, value: *Expression };

pub const Statement = union(enum) {
    var_declare: VarDeclare,
    var_assign: VarAssign,
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

    pub fn okAhead(self: *Parser, offset: u32) bool {
        return self.cursor + offset < self.tokens.len;
    }

    pub fn peekAhead(self: *Parser, offset: u32) Token {
        return self.tokens[self.cursor + offset];
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

    pub fn convertBinaryOperator(token: Token) ?BinaryOperator {
        return switch (token.kind) {
            .OperatorPlus => .Add,
            .OperatorMinus => .Subtract,
            .OperatorStar => .Multiply,
            .OperatorSlash => .Divide,
            else => null,
        };
    }

    pub fn parseType(self: *Parser) ?Type {
        const mutable = self.peek().kind == .KeywordMut;
        if (mutable) _ = self.consume();

        const kind: TypeKind = switch (self.peek().kind) {
            .KeywordU8 => .U8,
            .KeywordU16 => .U16,
            .KeywordU32 => .U32,
            .KeywordU64 => .U64,
            .KeywordI8 => .I8,
            .KeywordI16 => .I16,
            .KeywordI32 => .I32,
            .KeywordI64 => .I64,
            .KeywordF32 => .F32,
            .KeywordF64 => .F64,
            else => return null,
        };

        _ = self.consume();

        var t: Type = .{ .kind = kind, .mutable = mutable };

        while (self.peek().kind == .OperatorStar) {
            _ = self.consume();

            const k = self.allocator.create(TypeKind) catch unreachable;
            k.* = kind;

            t = .{ .kind = .{ .Pointer = k }, .mutable = mutable };
        }

        return t;
    }

    pub fn parseAtom(self: *Parser) *Expression {
        switch (self.peek().kind) {
            .Identifier => {
                if (self.cursor + 1 < self.tokens.len and self.tokens[self.cursor + 1].kind == .SymbolOpenParen)
                    return self.parseFnCall();

                const result = self.allocator.create(Expression) catch unreachable;
                result.* = .{ .identifier = self.consume().value };

                return result;
            },
            .LiteralNumber => {
                const result = self.allocator.create(Expression) catch unreachable;
                result.* = .{ .number_literal = std.fmt.parseFloat(f64, self.consume().value) catch unreachable };

                return result;
            },
            .SymbolOpenParen => {
                _ = self.consume();
                const expr = self.parseExpression();
                self.expect(.SymbolCloseParen);

                return expr;
            },
            else => {
                std.debug.print("expected expression: {any}\n", .{self.peek().kind});
                std.process.exit(0);
            },
        }
    }

    fn parseExpressionPrec(self: *Parser, min_prec: u32) *Expression {
        var lhs = self.parseAtom();

        while (self.ok()) {
            const op = Parser.convertBinaryOperator(self.peek()) orelse break;

            if (op.precedence() > min_prec)
                break;

            var next_min_prec = op.precedence();
            if (op.associativity() == 0) next_min_prec -= 1;

            _ = self.consume();

            const next_lhs = self.allocator.create(Expression) catch unreachable;
            next_lhs.* = .{ .binary = .{ .operator = op, .left = lhs, .right = self.parseExpressionPrec(next_min_prec) } };

            lhs = next_lhs;
        }

        return lhs;
    }

    pub fn parseExpression(self: *Parser) *Expression {
        return self.parseExpressionPrec(std.math.maxInt(u32));
    }

    pub fn appendExpression(self: *Parser) void {
        self.program.append(self.allocator, .{ .expression = self.parseExpression() }) catch unreachable;
    }

    pub fn parseVarDeclare(self: *Parser, typ: Type) void {
        const ident = self.consume();
        self.expect(.SymbolEqual);

        self.program.append(self.allocator, .{
            .var_declare = .{
                .type = typ,
                .name = ident.value,
                .value = self.parseExpression(),
            },
        }) catch unreachable;
    }

    pub fn parseVarAssign(self: *Parser) void {
        const ident = self.consume();
        self.expect(.SymbolEqual);

        self.program.append(self.allocator, .{ .var_assign = .{ .name = ident.value, .value = self.parseExpression() } }) catch unreachable;
    }

    pub fn parseFnCall(self: *Parser) *Expression {
        const name = self.consume().value;

        self.expect(.SymbolOpenParen);

        const expr = self.allocator.create(Expression) catch unreachable;
        expr.* = .{ .fn_call = .{ .name = name } };

        while (self.peek().kind != .SymbolCloseParen) {
            expr.*.fn_call.args.append(self.allocator, self.parseExpression()) catch unreachable;
            if (self.peek().kind != .SymbolCloseParen) self.expect(.SymbolComma);
        }

        self.expect(.SymbolCloseParen);

        return expr;
    }

    pub fn parse(self: *Parser) void {
        while (self.ok()) {
            switch (self.peek().kind) {
                .Identifier => {
                    if (self.okAhead(1) and self.peekAhead(1).kind == .SymbolEqual) {
                        self.parseVarAssign();
                    } else self.appendExpression();
                },
                else => {
                    if (self.parseType()) |t| {
                        self.parseVarDeclare(t);
                    } else self.appendExpression();
                },
            }

            self.expect(.SymbolSemiColon);
        }
    }
};
