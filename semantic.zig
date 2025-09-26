const std = @import("std");
const parse = @import("parse.zig");

const Expression = parse.Expression;
const VarDeclare = parse.VarDeclare;
const VarAssign = parse.VarAssign;
const Statement = parse.Statement;
const Type = parse.Type;

const Scope = struct { variables: std.StringHashMap(Type) };

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    program: std.ArrayList(Statement),
    scope_stack: std.ArrayList(Scope) = .empty,
    errors: std.ArrayList([]const u8) = .empty,

    pub fn init(allocator: std.mem.Allocator, program: std.ArrayList(Statement)) SemanticAnalyzer {
        return .{ .allocator = allocator, .program = program };
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

    pub fn analyze(self: *SemanticAnalyzer) void {
        self.enterScope();

        for (self.program.items) |s| {
            switch (s) {
                .var_declare => self.analyzeVarDeclare(s.var_declare),
                .var_assign => self.analyzeVarAssign(s.var_assign),
                .expression => self.analyzeExpression(s.expression),
            }
        }
    }
};
