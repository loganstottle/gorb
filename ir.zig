const std = @import("std");
const parse = @import("parse.zig");

const Type = parse.Type;
const BinaryOperator = parse.Type;
const Statement = parse.Statement;
const Expression = parse.Expression;

const VarDeclare = parse.VarDeclare;
const VarAssign = parse.VarAssign;

pub const Temp = struct {
    id: usize,
    //type: Type,
};

pub const Const = struct { dest: Temp, value: f64 }; // todo: add more literal types e.g. str
pub const Alloca = struct { name: []const u8 };
pub const Store = struct { dest: []const u8, src: Temp };
pub const Load = struct { dest: Temp, src: []const u8 };
pub const Call = struct { dest: Temp, name: []const u8, args: std.ArrayList(Temp) = .empty }; // todo: function signature
pub const BinaryOp = struct { dest: Temp, left: Temp, right: Temp };

pub const Instruction = union(enum) {
    @"const": Const,
    alloca: Alloca,
    store: Store,
    load: Load,
    call: Call,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    andand: BinaryOp,
    oror: BinaryOp,
    cmp_eq: BinaryOp,
    cmp_neq: BinaryOp,
    cmp_gt: BinaryOp,
    cmp_lt: BinaryOp,
    cmp_geq: BinaryOp,
    cmp_leq: BinaryOp,
};

pub const BasicBlock = struct {
    instructions: std.ArrayList(Instruction) = .empty,
    predecessors: std.ArrayList(BasicBlock) = .empty,
    successors: std.ArrayList(BasicBlock) = .empty,
};

pub const IREmitter = struct {
    allocator: std.mem.Allocator,
    block: BasicBlock,
    num_temps: usize = 0,

    pub fn init(allocator: std.mem.Allocator) IREmitter {
        return .{ .allocator = allocator, .block = .{} };
    }

    pub fn deinit(self: *IREmitter) void {
        for (self.block.instructions.items) |inst| {
            switch (inst) {
                .call => |c| @constCast(&c.args).deinit(self.allocator),
                else => {},
            }
        }

        self.block.instructions.deinit(self.allocator);
        self.block.predecessors.deinit(self.allocator);
        self.block.successors.deinit(self.allocator);
    }

    pub fn newTemp(self: *IREmitter) Temp {
        const id = self.num_temps;
        self.num_temps += 1;
        return .{ .id = id };
    }

    pub fn emitConst(self: *IREmitter, dest: Temp, value: f64) Temp {
        self.block.instructions.append(self.allocator, .{ .@"const" = .{ .dest = dest, .value = value } }) catch unreachable;
        return dest;
    }

    pub fn emitAlloca(self: *IREmitter, name: []const u8) void {
        self.block.instructions.append(self.allocator, .{ .alloca = .{ .name = name } }) catch unreachable;
    }

    pub fn emitStore(self: *IREmitter, dest: []const u8, src: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .store = .{ .dest = dest, .src = src } }) catch unreachable;
        return src;
    }

    pub fn emitLoad(self: *IREmitter, dest: Temp, src: []const u8) Temp {
        self.block.instructions.append(self.allocator, .{ .load = .{ .dest = dest, .src = src } }) catch unreachable;
        return dest;
    }

    pub fn emitCall(self: *IREmitter, dest: Temp, name: []const u8, args: std.ArrayList(Temp)) Temp {
        self.block.instructions.append(self.allocator, .{ .call = .{ .dest = dest, .name = name, .args = args } }) catch unreachable;
        return dest;
    }

    pub fn emitAdd(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .add = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitSub(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .sub = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitMul(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .mul = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitDiv(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .div = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpEq(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_eq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpNeq(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_neq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpGt(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_gt = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpLt(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_lt = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpGeq(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_geq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpLeq(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .cmp_leq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitAndAnd(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .andand = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitOrOr(self: *IREmitter, dest: Temp, left: Temp, right: Temp) Temp {
        self.block.instructions.append(self.allocator, .{ .oror = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitVarDeclare(self: *IREmitter, var_declare: VarDeclare) void {
        _ = self.emitAlloca(var_declare.name);
        _ = self.emitStore(var_declare.name, self.emitExpression(var_declare.value));
    }

    pub fn emitVarAssign(self: *IREmitter, var_assign: VarAssign) void {
        _ = self.emitStore(var_assign.name, self.emitExpression(var_assign.value));
    }

    pub fn emitExpression(self: *IREmitter, expression: *Expression) Temp {
        switch (expression.*) {
            .number_literal => |n| return self.emitConst(self.newTemp(), n),
            .identifier => |i| return self.emitLoad(self.newTemp(), i),
            .fn_call => |f| {
                var tmps: std.ArrayList(Temp) = .empty;
                for (f.args.items) |e| tmps.append(self.allocator, self.emitExpression(e)) catch unreachable;
                return self.emitCall(self.newTemp(), f.name, tmps);
            },
            .binary => |b| {
                const left = self.emitExpression(b.left);
                const right = self.emitExpression(b.right);

                switch (b.operator) {
                    .Add => return self.emitAdd(self.newTemp(), left, right),
                    .Subtract => return self.emitSub(self.newTemp(), left, right),
                    .Multiply => return self.emitMul(self.newTemp(), left, right),
                    .Divide => return self.emitDiv(self.newTemp(), left, right),
                    .Equal => return self.emitCmpEq(self.newTemp(), left, right),
                    .NotEqual => return self.emitCmpNeq(self.newTemp(), left, right),
                    .Greater => return self.emitCmpGt(self.newTemp(), left, right),
                    .Less => return self.emitCmpLt(self.newTemp(), left, right),
                    .GreaterEqual => return self.emitCmpGeq(self.newTemp(), left, right),
                    .LessEqual => return self.emitCmpLeq(self.newTemp(), left, right),
                    .OrOr => return self.emitOrOr(self.newTemp(), left, right),
                    .AndAnd => return self.emitAndAnd(self.newTemp(), left, right),
                }
            },
        }
    }

    pub fn emit(self: *IREmitter, statements: []Statement) void {
        for (statements) |statement| {
            switch (statement) {
                .var_declare => |d| self.emitVarDeclare(d),
                .var_assign => |a| self.emitVarAssign(a),
                .expression => |e| _ = self.emitExpression(e),
            }
        }
    }

    pub fn log(self: *IREmitter) void {
        for (self.block.instructions.items) |instruction| {
            switch (instruction) {
                .@"const" => |i| std.debug.print("t{} = const {}\n", .{ i.dest.id, i.value }),
                .alloca => |i| std.debug.print("{s} = alloca\n", .{i.name}),
                .store => |i| std.debug.print("{s} = store t{}\n", .{ i.dest, i.src.id }),
                .load => |i| std.debug.print("t{} = load {s}\n", .{ i.dest.id, i.src }),
                .call => |i| {
                    std.debug.print("t{} = call {s} ", .{ i.dest.id, i.name });

                    for (i.args.items) |arg| {
                        std.debug.print("t{} ", .{arg.id});
                    }

                    std.debug.print("\n", .{});
                },
                .add => |i| std.debug.print("t{} = add t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .sub => |i| std.debug.print("t{} = sub t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .mul => |i| std.debug.print("t{} = mul t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .div => |i| std.debug.print("t{} = div t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .andand => |i| std.debug.print("t{} = and t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .oror => |i| std.debug.print("t{} = or t{} t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_eq => |i| std.debug.print("t{} = cmp t{} == t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_neq => |i| std.debug.print("t{} = cmp t{} != t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_gt => |i| std.debug.print("t{} = cmp t{} > t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_lt => |i| std.debug.print("t{} = cmp t{} < t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_geq => |i| std.debug.print("t{} = cmp t{} >= t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
                .cmp_leq => |i| std.debug.print("t{} = cmp t{} <= t{}\n", .{ i.dest.id, i.left.id, i.right.id }),
            }
        }
    }
};
