const std = @import("std");
const parse = @import("parse.zig");

const Expression = parse.Expression;
const VarDeclare = parse.VarDeclare;
const VarAssign = parse.VarAssign;
const IfElse = parse.IfElse;
const LoopWhile = parse.LoopWhile;
const FnDeclare = parse.FnDeclare;
const Statement = parse.Statement;
const Type = parse.Type;

const Scope = struct { variables: std.StringHashMap(Type) };

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(FnDeclare),
    scope_stack: std.ArrayList(Scope) = .empty,
    errors: std.ArrayList([]const u8) = .empty,
    current_function: ?*FnDeclare = null,

    pub fn init(allocator: std.mem.Allocator, functions: std.ArrayList(FnDeclare)) SemanticAnalyzer {
        return .{ .allocator = allocator, .functions = functions };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        for (self.scope_stack.items) |*s|
            s.variables.deinit();

        self.scope_stack.deinit(self.allocator);
    }

    pub fn err(self: *SemanticAnalyzer, comptime msg: []const u8, args: anytype) void {
        const str = std.fmt.allocPrint(self.allocator, msg, args) catch unreachable;
        self.errors.append(self.allocator, str) catch unreachable;
    }

    pub fn addSymbol(self: *SemanticAnalyzer, name: []const u8, typ: Type) void {
        self.scope_stack.items[self.scope_stack.items.len - 1].variables.put(name, typ) catch unreachable;
    }

    pub fn getSymbol(self: *SemanticAnalyzer, name: []const u8) ?Type {
        const num_scopes = self.scope_stack.items.len;

        var i: usize = 0;
        while (i < num_scopes) : (i += 1) {
            if (self.scope_stack.items[num_scopes - i - 1].variables.get(name)) |typ|
                return typ;
        }

        return null;
    }

    pub fn enterScope(self: *SemanticAnalyzer) void {
        self.scope_stack.append(self.allocator, .{ .variables = std.StringHashMap(Type).init(self.allocator) }) catch unreachable;
    }

    pub fn exitScope(self: *SemanticAnalyzer) void {
        self.scope_stack.items[self.scope_stack.items.len - 1].variables.deinit();
        _ = self.scope_stack.pop();
    }

    pub fn analyzeVarDeclare(self: *SemanticAnalyzer, var_declare: VarDeclare) void {
        self.analyzeExpression(var_declare.value);

        if (self.getSymbol(var_declare.name) != null) {
            self.err("\"{s}\" has already been declared\n", .{var_declare.name});
            return;
        }

        var typ = var_declare.type;
        if (!var_declare.value.ty.same(&typ)) {
            self.err("\"{s}\" has type {any} but assigned value of type {any}\n", .{ var_declare.name, var_declare.type, var_declare.value.ty });
            return;
        }

        self.addSymbol(var_declare.name, var_declare.type);
    }

    pub fn analyzeVarAssign(self: *SemanticAnalyzer, var_assign: VarAssign) void {
        self.analyzeExpression(var_assign.value);

        var typ = self.getSymbol(var_assign.name);

        if (typ == null) {
            self.err("\"{s}\" is not defined\n", .{var_assign.name});
            return;
        }

        if (!var_assign.value.ty.same(&typ.?)) {
            self.err("\"{s}\" has type {any} but assigned value of type {any}\n", .{ var_assign.name, typ.?, var_assign.value.ty });
            return;
        }
    }

    pub fn analyzeExpression(self: *SemanticAnalyzer, expression: *Expression) void {
        switch (expression.kind) {
            .identifier => |e| {
                const ty = self.getSymbol(e);

                if (ty == null) {
                    self.err("\"{s}\" is not defined\n", .{e});
                    return;
                }

                expression.ty = ty.?;
            },
            .binary => |*e| {
                self.analyzeExpression(e.left);
                self.analyzeExpression(e.right);

                switch (e.operator) {
                    .Add, .Subtract, .Multiply, .Divide => {
                        if (e.left.ty == .I8 and e.right.ty == .I8) {
                            expression.ty = .I8;
                            return;
                        }

                        if (e.left.ty == .I16 and e.right.ty == .I16) {
                            expression.ty = .I16;
                            return;
                        }

                        if (e.left.ty == .I32 and e.right.ty == .I32) {
                            expression.ty = .I32;
                            return;
                        }

                        if (e.left.ty == .I64 and e.right.ty == .I64) {
                            expression.ty = .I64;
                            return;
                        }

                        if (e.left.ty == .U8 and e.right.ty == .U8) {
                            expression.ty = .U8;
                            return;
                        }

                        if (e.left.ty == .U16 and e.right.ty == .U16) {
                            expression.ty = .U16;
                            return;
                        }

                        if (e.left.ty == .U32 and e.right.ty == .U32) {
                            expression.ty = .U32;
                            return;
                        }

                        if (e.left.ty == .U64 and e.right.ty == .U64) {
                            expression.ty = .U64;
                            return;
                        }

                        if (e.left.ty == .F32 and e.right.ty == .F32) {
                            expression.ty = .F32;
                            return;
                        }

                        if (e.left.ty == .F64 and e.right.ty == .F64) {
                            expression.ty = .F64;
                            return;
                        }

                        self.err("operator {any} requires numeric operands of the same type, got left = {any}, right = {any}\n", .{ e.operator, e.left.ty, e.right.ty });
                    },
                    .Equal, .NotEqual => {
                        if (!e.left.ty.same(&e.right.ty)) {
                            self.err("operator {any} requires operands of the same type\n", .{e.operator});
                            return;
                        }

                        if (e.left.ty == .F32 or e.left.ty == .F64) {
                            self.err("operator {any}: equality of floats will not work as expected\n", .{e.operator});
                            return;
                        }

                        expression.ty = .Boolean;
                    },
                    .Greater, .GreaterEqual, .Less, .LessEqual => {
                        if (!e.left.ty.same(&e.right.ty)) {
                            self.err("operator {any} requires operands of the same type\n", .{e.operator});
                            return;
                        }

                        expression.ty = .Boolean;
                    },
                    .OrOr, .AndAnd => {
                        if (e.left.ty != .Boolean or e.right.ty != .Boolean)
                            self.err("operator {any} requires boolean operands, got left = {any}, right = {any}\n", .{ e.operator, e.left.ty, e.right.ty });

                        expression.ty = .Boolean;
                    },
                }
            },
            .fn_call => |*e| {
                var func: ?*FnDeclare = null;

                for (self.functions.items) |*f| {
                    if (std.mem.eql(u8, e.name, f.signature.name)) {
                        func = f;
                        break;
                    }
                }

                if (func == null) {
                    self.err("function not found\n", .{});
                    return;
                }

                expression.ty = func.?.signature.return_type;

                if (func.?.signature.parameters.items.len != e.args.items.len) {
                    self.err("incorrect number of function arguments provided\n", .{});
                    return;
                }

                for (e.args.items) |arg| self.analyzeExpression(arg);

                for (e.args.items, 0..) |arg, idx| {
                    var expected_arg_ty = func.?.signature.parameters.items[idx].type;

                    if (!arg.ty.same(&expected_arg_ty)) {
                        self.err("incorrect argument type, expected {}, got {}\n", .{ expected_arg_ty, arg.ty });
                        return;
                    }
                }
            },
            .number_literal => expression.ty = .I32, //todo: add more number literals
        }
    }

    pub fn analyzeIfElse(self: *SemanticAnalyzer, if_else: IfElse) void {
        self.analyzeExpression(if_else.condition);

        if (if_else.condition.ty != .Boolean) {
            self.err("if requires boolean condition, got {any}\n", .{if_else.condition.ty});
            return;
        }

        for (if_else.if_block.items) |s| self.analyzeStatement(s);
        for (if_else.else_block.items) |s| self.analyzeStatement(s);
    }

    pub fn analyzeWhile(self: *SemanticAnalyzer, loop_while: LoopWhile) void {
        self.analyzeExpression(loop_while.condition);

        if (loop_while.condition.ty != .Boolean) {
            self.err("while requires boolean condition, got {any}\n", .{loop_while.condition.ty});
            return;
        }

        for (loop_while.body.items) |s| self.analyzeStatement(s);
    }

    pub fn analyzeReturn(self: *SemanticAnalyzer, expression: *Expression) void {
        self.analyzeExpression(expression);

        var return_type = self.current_function.?.signature.return_type;

        if (!return_type.same(&expression.ty))
            self.err("incorrect return type, expected {any}, got {any}", .{ return_type, expression.ty });
    }

    pub fn analyzeStatement(self: *SemanticAnalyzer, statement: Statement) void {
        switch (statement) {
            .expression => |st| self.analyzeExpression(st),
            .var_declare => |st| self.analyzeVarDeclare(st),
            .var_assign => |st| self.analyzeVarAssign(st),
            .if_else => |st| self.analyzeIfElse(st),
            .loop_while => |st| self.analyzeWhile(st),
            .@"return" => |st| self.analyzeReturn(st),
        }
    }

    pub fn analyze(self: *SemanticAnalyzer) void {
        for (self.functions.items) |*f| {
            self.current_function = f;

            self.enterScope();

            for (f.signature.parameters.items) |p| self.addSymbol(p.name, p.type);
            for (f.body.items) |s| self.analyzeStatement(s);

            self.exitScope();

            self.current_function = null;
        }
    }
};
