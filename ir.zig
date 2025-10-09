const std = @import("std");
const parse = @import("parse.zig");

const Type = parse.Type;
const BinaryOperator = parse.Type;
const Statement = parse.Statement;

const Expression = parse.Expression;
const VarDeclare = parse.VarDeclare;
const VarAssign = parse.VarAssign;
const IfElse = parse.IfElse;
const LoopWhile = parse.LoopWhile;
const FnDeclare = parse.FnDeclare;

pub const Temp = struct {
    id: usize,
    //type: Type,
};

pub const Const = struct { dest: Temp, value: f64 }; // todo: add more literal types e.g. bool, str
pub const Alloca = struct { name: []const u8 };
pub const Store = struct { dest: []const u8, src: Temp };
pub const Load = struct { dest: Temp, src: []const u8 };
pub const Call = struct { dest: Temp, name: []const u8, args: std.ArrayList(Temp) = .empty }; // todo: function signature
pub const BinaryOp = struct { dest: Temp, left: Temp, right: Temp };
pub const BranchIf = struct { condition: Temp, true_block: usize, false_block: usize };

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
    branch: usize,
    branch_if: BranchIf,
};

pub const BasicBlock = struct {
    id: usize,
    instructions: std.ArrayList(Instruction) = .empty,
    predecessors: std.ArrayList(usize) = .empty,
    successors: std.ArrayList(usize) = .empty,

    pub fn then(self: *BasicBlock, allocator: std.mem.Allocator, other: *BasicBlock) void {
        self.successors.append(allocator, other.id) catch unreachable;
        other.predecessors.append(allocator, self.id) catch unreachable;
    }

    pub fn terminator(self: *BasicBlock) *Instruction {
        return &self.instructions.items[self.instructions.items.len - 1];
    }
};

pub const ControlFlowGraph = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    blocks: std.ArrayList(BasicBlock) = .empty,
    current_block: usize = 0,
    num_temps: usize = 0,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) ControlFlowGraph {
        return ControlFlowGraph{ .allocator = allocator, .name = name };
    }

    pub fn deinit(self: *ControlFlowGraph) void {
        for (self.blocks.items) |block| {
            for (block.instructions.items) |inst| {
                switch (inst) {
                    .call => |c| @constCast(&c.args).deinit(self.allocator),
                    else => {},
                }
            }

            @constCast(&block.instructions).deinit(self.allocator);
            @constCast(&block.predecessors).deinit(self.allocator);
            @constCast(&block.successors).deinit(self.allocator);
        }

        self.blocks.deinit(self.allocator);
    }

    pub fn newTemp(self: *ControlFlowGraph) Temp {
        const id = self.num_temps;
        self.num_temps += 1;
        return .{ .id = id };
    }

    pub fn appendBlock(self: *ControlFlowGraph) usize {
        const id = self.blocks.items.len;
        self.blocks.append(self.allocator, BasicBlock{ .id = id }) catch unreachable;
        return id;
    }

    pub fn emitConst(self: *ControlFlowGraph, dest: Temp, value: f64) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .@"const" = .{ .dest = dest, .value = value } }) catch unreachable;
        return dest;
    }

    pub fn emitAlloca(self: *ControlFlowGraph, name: []const u8) void {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .alloca = .{ .name = name } }) catch unreachable;
    }

    pub fn emitStore(self: *ControlFlowGraph, dest: []const u8, src: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .store = .{ .dest = dest, .src = src } }) catch unreachable;
        return src;
    }

    pub fn emitLoad(self: *ControlFlowGraph, dest: Temp, src: []const u8) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .load = .{ .dest = dest, .src = src } }) catch unreachable;
        return dest;
    }

    pub fn emitCall(self: *ControlFlowGraph, dest: Temp, name: []const u8, args: std.ArrayList(Temp)) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .call = .{ .dest = dest, .name = name, .args = args } }) catch unreachable;
        return dest;
    }

    pub fn emitAdd(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .add = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitSub(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .sub = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitMul(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .mul = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitDiv(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .div = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpEq(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_eq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpNeq(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_neq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpGt(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_gt = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpLt(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_lt = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpGeq(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_geq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitCmpLeq(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .cmp_leq = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitAndAnd(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .andand = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitOrOr(self: *ControlFlowGraph, dest: Temp, left: Temp, right: Temp) Temp {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .oror = .{ .dest = dest, .left = left, .right = right } }) catch unreachable;
        return dest;
    }

    pub fn emitBranch(self: *ControlFlowGraph, block_id: usize) void {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .branch = block_id }) catch unreachable;
    }

    pub fn emitBranchIf(self: *ControlFlowGraph, condition: Temp, true_block_id: usize, false_block_id: usize) void {
        self.blocks.items[self.current_block].instructions.append(self.allocator, .{ .branch_if = .{ .condition = condition, .true_block = true_block_id, .false_block = false_block_id } }) catch unreachable;
    }

    pub fn emitVarDeclare(self: *ControlFlowGraph, var_declare: VarDeclare) void {
        self.emitAlloca(var_declare.name);
        _ = self.emitStore(var_declare.name, self.emitExpression(var_declare.value));
    }

    pub fn emitVarAssign(self: *ControlFlowGraph, var_assign: VarAssign) void {
        _ = self.emitStore(var_assign.name, self.emitExpression(var_assign.value));
    }

    pub fn emitExpression(self: *ControlFlowGraph, expression: *Expression) Temp {
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

    pub fn emitIf(self: *ControlFlowGraph, if_else: IfElse) void {
        const start_block = self.current_block;

        const if_block = self.appendBlock();
        self.emitBranchIf(self.emitExpression(if_else.condition), if_block, 0);

        self.current_block = if_block;
        for (if_else.if_block.items) |statement| self.emitStatement(statement);

        const merge_block = self.appendBlock();
        self.emitBranch(merge_block);
        self.blocks.items[self.current_block].then(self.allocator, &self.blocks.items[merge_block]);

        self.current_block = merge_block;
        self.blocks.items[start_block].terminator().branch_if.false_block = merge_block;

        self.blocks.items[start_block].then(self.allocator, &self.blocks.items[if_block]);
        self.blocks.items[start_block].then(self.allocator, &self.blocks.items[merge_block]);
    }

    pub fn emitIfElse(self: *ControlFlowGraph, if_else: IfElse) void {
        const start_block = self.current_block;

        const if_block = self.appendBlock();
        self.emitBranchIf(self.emitExpression(if_else.condition), if_block, 0);

        self.current_block = if_block;
        for (if_else.if_block.items) |statement| self.emitStatement(statement);
        self.emitBranch(0);

        const else_block = self.appendBlock();

        self.blocks.items[start_block].terminator().branch_if.false_block = else_block;
        self.current_block = else_block;
        for (if_else.else_block.items) |statement| self.emitStatement(statement);

        const merge_block = self.appendBlock();
        self.emitBranch(merge_block);

        self.blocks.items[self.current_block].then(self.allocator, &self.blocks.items[merge_block]);

        self.current_block = merge_block;
        self.blocks.items[else_block - 1].terminator().branch = merge_block;
        self.blocks.items[else_block - 1].then(self.allocator, &self.blocks.items[merge_block]);

        self.blocks.items[start_block].then(self.allocator, &self.blocks.items[if_block]);
        self.blocks.items[start_block].then(self.allocator, &self.blocks.items[else_block]);
    }

    pub fn emitConditional(self: *ControlFlowGraph, conditional: IfElse) void {
        if (conditional.else_block.items.len == 0) return self.emitIf(conditional);
        self.emitIfElse(conditional);
    }

    pub fn emitWhile(self: *ControlFlowGraph, loop_while: LoopWhile) void {
        const start_block = self.current_block;

        const cond_block = self.appendBlock();
        const body_block = self.appendBlock();

        self.emitBranch(cond_block);

        self.current_block = cond_block;
        self.emitBranchIf(self.emitExpression(loop_while.condition), body_block, 0);

        self.current_block = body_block;
        for (loop_while.body.items) |statement| self.emitStatement(statement);
        self.blocks.items[self.current_block].then(self.allocator, &self.blocks.items[cond_block]);
        self.emitBranch(cond_block);

        const merge_block = self.appendBlock();
        self.blocks.items[cond_block].terminator().branch_if.false_block = merge_block;
        self.current_block = merge_block;

        self.blocks.items[start_block].then(self.allocator, &self.blocks.items[cond_block]);
        self.blocks.items[cond_block].then(self.allocator, &self.blocks.items[body_block]);
        self.blocks.items[cond_block].then(self.allocator, &self.blocks.items[merge_block]);
    }

    pub fn emitStatement(self: *ControlFlowGraph, statement: Statement) void {
        switch (statement) {
            .expression => |s| _ = self.emitExpression(s),
            .var_declare => |s| self.emitVarDeclare(s),
            .var_assign => |s| self.emitVarAssign(s),
            .if_else => |s| self.emitConditional(s),
            .loop_while => |s| self.emitWhile(s),
        }
    }

    pub fn emit(self: *ControlFlowGraph, statements: []Statement) void {
        _ = self.appendBlock();
        for (statements) |statement| self.emitStatement(statement);
    }

    pub fn log(self: *ControlFlowGraph) void {
        for (self.blocks.items) |block| {
            if (block.id > 0) std.debug.print("{s}.{}:\n", .{ self.name, block.id });

            for (block.instructions.items) |instruction| {
                std.debug.print("  ", .{});

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
                    .branch => |i| std.debug.print("br {s}.{}\n", .{ self.name, i }),
                    .branch_if => |i| std.debug.print("if t{} br {s}.{} else {s}.{}\n", .{ i.condition.id, self.name, i.true_block, self.name, i.false_block }),
                }
            }
        }
    }

    pub fn dot(self: *ControlFlowGraph, path: []const u8) void {
        var file = std.fs.cwd().createFile(path, .{}) catch unreachable;
        defer file.close();

        file.writeAll("digraph {\n") catch unreachable;

        for (self.blocks.items) |block| {
            const buf = std.fmt.allocPrint(self.allocator, "  {s}_{} [label=\"{s}.{}\"]\n", .{ self.name, block.id, self.name, block.id }) catch unreachable;
            defer self.allocator.free(buf);

            file.writeAll(buf) catch unreachable;
        }

        for (self.blocks.items) |block| {
            for (block.successors.items) |succ| {
                const buf = std.fmt.allocPrint(self.allocator, "  {s}_{} -> {s}_{}\n", .{ self.name, block.id, self.name, succ }) catch unreachable;
                defer self.allocator.free(buf);

                file.writeAll(buf) catch unreachable;
            }
        }

        file.writeAll("}") catch unreachable;
    }
};

pub const IRModule = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(ControlFlowGraph) = .empty,

    pub fn init(allocator: std.mem.Allocator) IRModule {
        return IRModule{ .allocator = allocator };
    }

    pub fn deinit(self: *IRModule) void {
        for (self.functions.items) |*func| func.deinit();
        self.functions.deinit(self.allocator);
    }

    pub fn emit(self: *IRModule, functions: []FnDeclare) void {
        for (functions) |func| {
            var function = ControlFlowGraph.init(self.allocator, func.signature.name);
            function.emit(func.body.items);
            self.functions.append(self.allocator, function) catch unreachable;
        }
    }

    pub fn log(self: *IRModule) void {
        for (self.functions.items) |*func| {
            std.debug.print("\n{s}:\n", .{func.name});
            func.log();
        }
    }
};
