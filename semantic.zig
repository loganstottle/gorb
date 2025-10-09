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

    pub fn init(allocator: std.mem.Allocator, functions: std.ArrayList(FnDeclare)) SemanticAnalyzer {
        return .{ .allocator = allocator, .functions = functions };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        for (self.scope_stack.items) |*s|
            s.variables.deinit();

        self.scope_stack.deinit(self.allocator);
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
        if (self.getSymbol(var_declare.name) != null)
            std.debug.print("\"{s}\" has already been declared\n", .{var_declare.name});

        self.addSymbol(var_declare.name, var_declare.type);
        self.analyzeExpression(var_declare.value);
    }

    pub fn analyzeVarAssign(self: *SemanticAnalyzer, var_assign: VarAssign) void {
        if (self.getSymbol(var_assign.name)) |typ| {
            if (!typ.mutable)
                std.debug.print("cannot reassign immutable variable \"{s}\"\n", .{var_assign.name});
        } else std.debug.print("\"{s}\" is not defined\n", .{var_assign.name});

        self.analyzeExpression(var_assign.value);
    }

    pub fn analyzeExpression(self: *SemanticAnalyzer, expression: *Expression) void {
        switch (expression.*) {
            .identifier => |e| {
                if (self.getSymbol(e) == null)
                    std.debug.print("\"{s}\" is not defined\n", .{e});
            },
            .binary => |e| {
                self.analyzeExpression(e.left);
                self.analyzeExpression(e.right);
            },
            .fn_call => |e| for (e.args.items) |arg| self.analyzeExpression(arg),
            else => {},
        }
    }

    pub fn analyzeIfElse(self: *SemanticAnalyzer, if_else: IfElse) void {
        _ = self;
        _ = if_else;
    }

    pub fn analyzeWhile(self: *SemanticAnalyzer, loop_while: LoopWhile) void {
        _ = self;
        _ = loop_while;
    }

    pub fn analyzeFnDeclare(self: *SemanticAnalyzer, fn_declare: FnDeclare) void {
        _ = self;
        _ = fn_declare;
    }

    pub fn analyze(self: *SemanticAnalyzer) void {
        for (self.functions.items) |f| {
            self.enterScope();

            for (f.body.items) |s| {
                switch (s) {
                    .expression => |st| self.analyzeExpression(st),
                    .var_declare => |st| self.analyzeVarDeclare(st),
                    .var_assign => |st| self.analyzeVarAssign(st),
                    .if_else => |st| self.analyzeIfElse(st),
                    .loop_while => |st| self.analyzeWhile(st),
                }
            }

            self.exitScope();
        }
    }
};
