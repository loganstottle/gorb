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
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    OrOr,
    AndAnd,

    pub fn precedence(self: BinaryOperator) u8 {
        return switch (self) {
            .OrOr => 6,
            .AndAnd => 5,
            .Equal, .NotEqual => 4,
            .Greater, .Less, .GreaterEqual, .LessEqual => 3,
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

pub const Parameter = struct {
    type: Type,
    name: []const u8,
};

pub const IfElse = struct {
    condition: *Expression,
    if_block: std.ArrayList(Statement) = .empty,
    else_block: std.ArrayList(Statement) = .empty,
};

pub const LoopWhile = struct {
    condition: *Expression,
    body: std.ArrayList(Statement) = .empty,
};

pub const FnSignature = struct {
    name: []const u8,
    return_type: Type,
    parameters: std.ArrayList(Parameter) = .empty,
};

pub const FnDeclare = struct {
    signature: FnSignature,
    body: std.ArrayList(Statement) = .empty,

    pub fn deinit(self: *FnDeclare, allocator: std.mem.Allocator) void {
        self.signature.parameters.deinit(allocator);
        for (self.body.items) |st| st.deinit(allocator);
        self.body.deinit(allocator);
    }
};

pub const Statement = union(enum) {
    expression: *Expression,
    var_declare: VarDeclare,
    var_assign: VarAssign,
    if_else: IfElse,
    loop_while: LoopWhile,

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        switch (self) {
            .expression => |s| s.deinit(allocator),
            .var_declare => |s| s.value.deinit(allocator),
            .var_assign => |s| s.value.deinit(allocator),
            .if_else => |s| {
                s.condition.deinit(allocator);

                for (s.if_block.items) |st| st.deinit(allocator);
                for (s.else_block.items) |st| st.deinit(allocator);

                @constCast(&s.if_block).deinit(allocator);
                @constCast(&s.else_block).deinit(allocator);
            },
            .loop_while => |s| {
                s.condition.deinit(allocator);
                for (s.body.items) |st| st.deinit(allocator);
                @constCast(&s.body).deinit(allocator);
            },
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    cursor: u32 = 0,
    functions: std.ArrayList(FnDeclare) = .empty,

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return .{ .allocator = allocator, .tokens = tokens };
    }

    pub fn deinit(self: *Parser) void {
        for (self.functions.items) |*func| func.deinit(self.allocator);
        self.functions.deinit(self.allocator);
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

    pub fn expect(self: *Parser, kind: TokenKind) Token {
        if (!self.ok()) {
            std.debug.print("expected {any}, found EOF\n", .{kind});
            std.process.exit(0);
        }

        const tok = self.consume();

        if (tok.kind != kind) {
            std.debug.print("expected {any}, found {any}\n", .{ kind, tok.kind });
            std.process.exit(0);
        }

        return tok;
    }

    pub fn convertBinaryOperator(token: Token) ?BinaryOperator {
        return switch (token.kind) {
            .OperatorPlus => .Add,
            .OperatorMinus => .Subtract,
            .OperatorStar => .Multiply,
            .OperatorSlash => .Divide,
            .SymbolEqualEqual => .Equal,
            .SymbolBangEqual => .NotEqual,
            .SymbolGreater => .Greater,
            .SymbolGreaterEqual => .GreaterEqual,
            .SymbolLess => .Less,
            .SymbolLessEqual => .LessEqual,
            .SymbolOrOr => .OrOr,
            .SymbolAndAnd => .AndAnd,
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
            .KeywordVoid => .Void,
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
                _ = self.expect(.SymbolCloseParen);

                return expr;
            },
            else => {
                std.debug.print("expected expression: {any}\n", .{self.peek().kind});
                std.process.exit(0);
            },
        }
    }

    pub fn parseBlock(self: *Parser) std.ArrayList(Statement) {
        var block: std.ArrayList(Statement) = .empty;

        if (self.peek().kind == .SymbolOpenCurly) {
            _ = self.expect(.SymbolOpenCurly);

            while (self.peek().kind != .SymbolCloseCurly)
                block.append(self.allocator, self.parseStatement()) catch unreachable;

            _ = self.expect(.SymbolCloseCurly);
        } else block.append(self.allocator, self.parseStatement()) catch unreachable;

        return block;
    }

    pub fn parseIfElse(self: *Parser) Statement {
        _ = self.expect(.KeywordIf);

        const condition = self.parseExpression();
        const if_block = self.parseBlock();
        var else_block: std.ArrayList(Statement) = .empty;

        if (self.ok() and self.peek().kind == .KeywordElse) {
            _ = self.consume();
            else_block = self.parseBlock();
        }

        return .{ .if_else = .{ .condition = condition, .if_block = if_block, .else_block = else_block } };
    }

    pub fn parseWhile(self: *Parser) Statement {
        _ = self.expect(.KeywordWhile);
        return .{ .loop_while = .{ .condition = self.parseExpression(), .body = self.parseBlock() } };
    }

    pub fn parseFnDeclare(self: *Parser) FnDeclare {
        _ = self.expect(.KeywordFn);

        const return_type = self.parseType().?;

        const name = self.expect(.Identifier).value;
        var parameters: std.ArrayList(Parameter) = .empty;

        _ = self.expect(.SymbolOpenParen);

        while (self.peek().kind != .SymbolCloseParen) {
            parameters.append(self.allocator, .{ .type = self.parseType().?, .name = self.expect(.Identifier).value }) catch unreachable;
            if (self.peek().kind != .SymbolCloseParen) _ = self.expect(.SymbolComma);
        }

        _ = self.expect(.SymbolCloseParen);

        return .{ .signature = .{ .name = name, .return_type = return_type, .parameters = parameters }, .body = self.parseBlock() };
    }

    pub fn parseExpressionPrec(self: *Parser, min_prec: u32) *Expression {
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

    pub fn parseVarDeclare(self: *Parser, typ: Type) Statement {
        const ident = self.consume();
        _ = self.expect(.SymbolEqual);

        return .{
            .var_declare = .{
                .type = typ,
                .name = ident.value,
                .value = self.parseExpression(),
            },
        };
    }

    pub fn parseVarAssign(self: *Parser) Statement {
        const ident = self.consume();
        _ = self.expect(.SymbolEqual);

        return .{ .var_assign = .{ .name = ident.value, .value = self.parseExpression() } };
    }

    pub fn parseFnCall(self: *Parser) *Expression {
        const name = self.consume().value;

        _ = self.expect(.SymbolOpenParen);

        const expr = self.allocator.create(Expression) catch unreachable;
        expr.* = .{ .fn_call = .{ .name = name } };

        while (self.peek().kind != .SymbolCloseParen) {
            expr.*.fn_call.args.append(self.allocator, self.parseExpression()) catch unreachable;
            if (self.peek().kind != .SymbolCloseParen) _ = self.expect(.SymbolComma);
        }

        _ = self.expect(.SymbolCloseParen);

        return expr;
    }

    pub fn parseStatement(self: *Parser) Statement {
        var statement: ?Statement = null;

        switch (self.peek().kind) {
            .KeywordIf => return self.parseIfElse(),
            .KeywordWhile => return self.parseWhile(),
            .Identifier => {
                if (self.okAhead(1) and self.peekAhead(1).kind == .SymbolEqual)
                    statement = self.parseVarAssign();
            },
            else => {
                if (self.parseType()) |t|
                    statement = self.parseVarDeclare(t);
            },
        }

        if (statement == null)
            statement = .{ .expression = self.parseExpression() };

        _ = self.expect(.SymbolSemiColon);

        return statement.?;
    }

    pub fn parse(self: *Parser) void {
        while (self.ok()) {
            switch (self.peek().kind) {
                .KeywordFn => self.functions.append(self.allocator, self.parseFnDeclare()) catch unreachable,
                else => {
                    std.debug.print("expected function declaration\n", .{});
                    std.process.exit(0);
                },
            }
        }
    }
};
