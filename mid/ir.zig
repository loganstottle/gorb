const std = @import("std");
const parse = @import("../front/parse.zig");

const Type = parse.Type;
const Statement = parse.Statement;

const Expression = parse.Expression;
const VarDeclare = parse.VarDeclare;
const VarAssign = parse.VarAssign;
const IfElse = parse.IfElse;
const LoopWhile = parse.LoopWhile;
const FnDeclare = parse.FnDeclare;
const FnSignature = parse.FnSignature;

pub const Temp = struct {
    id: usize,
    ty: Type,
};

pub const Const = struct { dest: Temp, value: i64 }; // todo: add more literal types e.g. bool, str
pub const Alloca = struct { name: []const u8, ty: Type };
pub const Store = struct { dest: []const u8, src: Temp };
pub const Load = struct { dest: Temp, src: []const u8 };
pub const Param = struct { dest: Temp, src: []const u8, idx: usize };
pub const Call = struct { dest: Temp, name: []const u8, args: std.ArrayList(Temp) = .empty }; // todo: function signature
pub const BinaryOp = struct { dest: Temp, left: Temp, right: Temp };
pub const BranchIf = struct { condition: Temp, true_block: usize, false_block: usize };

pub const Instruction = union(enum) {
    ret: Temp,
    @"const": Const,
    alloca: Alloca,
    store: Store,
    load: Load,
    param: Param,
    call: Call,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    andand: BinaryOp,
    oror: BinaryOp,
    cmp_eq: BinaryOp,
    cmp_ne: BinaryOp,
    cmp_gt: BinaryOp,
    cmp_lt: BinaryOp,
    cmp_ge: BinaryOp,
    cmp_le: BinaryOp,
    branch: usize,
    branch_if: BranchIf,
    dead,
};

pub const BasicBlock = struct {
    id: usize,
    instructions: std.ArrayList(usize) = .empty,
    predecessors: std.ArrayList(usize) = .empty,
    successors: std.ArrayList(usize) = .empty,

    pub fn then(self: *BasicBlock, allocator: std.mem.Allocator, other: *BasicBlock) void {
        self.successors.append(allocator, other.id) catch unreachable;
        other.predecessors.append(allocator, self.id) catch unreachable;
    }

    pub fn terminator(self: *BasicBlock, instructions: std.ArrayList(Instruction)) *Instruction {
        return &instructions.items[self.instructions.getLast()];
    }

    pub fn dbg(self: *BasicBlock) void {
        std.debug.print("\nDBG: bb{}\n", .{self.id});

        std.debug.print("predecessors: ", .{});
        for (self.predecessors.items) |pred|
            std.debug.print("bb{} ", .{pred});

        if (self.predecessors.items.len == 0) std.debug.print("(none)", .{});

        std.debug.print("\nsuccessors: ", .{});
        for (self.successors.items) |succ|
            std.debug.print("bb{} ", .{succ});

        if (self.successors.items.len == 0) std.debug.print("(none)", .{});

        std.debug.print("\n", .{});
    }
};

pub const IRFunction = struct {
    allocator: std.mem.Allocator,
    signature: FnSignature,
    block_pool: std.ArrayList(BasicBlock) = .empty,
    blocks: std.ArrayList(usize) = .empty,
    instruction_pool: std.ArrayList(Instruction) = .empty,
    current_block: usize = 0,
    num_temps: usize = 0,
    vars: std.ArrayList([]const u8) = .empty,

    pub fn init(allocator: std.mem.Allocator, signature: FnSignature) IRFunction {
        return IRFunction{ .allocator = allocator, .signature = signature };
    }

    pub fn deinit(self: *IRFunction) void {
        for (self.blocks.items) |block_id| {
            var block = self.getBlock(block_id);

            for (block.instructions.items) |inst_id| {
                const inst = self.instruction_pool.items[inst_id];

                switch (inst) {
                    .call => |*c| @constCast(&c.args).deinit(self.allocator),
                    else => {},
                }
            }

            block.instructions.deinit(self.allocator);
            block.predecessors.deinit(self.allocator);
            block.successors.deinit(self.allocator);
        }

        self.blocks.deinit(self.allocator);
    }

    pub fn dbg(self: *IRFunction) void {
        for (self.blocks.items) |block_id| self.getBlock(block_id).dbg();
        std.debug.print("\n", .{});
    }

    pub fn newTemp(self: *IRFunction, ty: Type) Temp {
        const id = self.num_temps;
        self.num_temps += 1;
        return .{ .id = id, .ty = ty };
    }

    pub fn getBlock(self: *IRFunction, block_id: usize) *BasicBlock {
        return &self.block_pool.items[block_id];
    }

    pub fn currentBlock(self: *IRFunction) *BasicBlock {
        return self.getBlock(self.current_block);
    }

    pub fn appendBlock(self: *IRFunction) usize {
        const id = self.block_pool.items.len;

        self.block_pool.append(self.allocator, .{ .id = id }) catch unreachable;
        self.blocks.append(self.allocator, id) catch unreachable;

        return id;
    }

    pub fn appendInstruction(self: *IRFunction, instruction: Instruction) void {
        self.currentBlock().instructions.append(self.allocator, self.instruction_pool.items.len) catch unreachable;
        self.instruction_pool.append(self.allocator, instruction) catch unreachable;
    }

    pub fn emitConst(self: *IRFunction, dest: Temp, value: i64) Temp {
        self.appendInstruction(.{ .@"const" = .{ .dest = dest, .value = value } });
        return dest;
    }

    pub fn emitAlloca(self: *IRFunction, name: []const u8, ty: Type) void {
        self.appendInstruction(.{ .alloca = .{ .name = name, .ty = ty } });
        self.vars.append(self.allocator, name) catch unreachable;
    }

    pub fn emitStore(self: *IRFunction, dest: []const u8, src: Temp) Temp {
        self.appendInstruction(.{ .store = .{ .dest = dest, .src = src } });
        return src;
    }

    pub fn emitLoad(self: *IRFunction, dest: Temp, src: []const u8) Temp {
        self.appendInstruction(.{ .load = .{ .dest = dest, .src = src } });
        return dest;
    }

    pub fn emitParam(self: *IRFunction, dest: Temp, src: []const u8, idx: usize) Temp {
        self.appendInstruction(.{ .param = .{ .dest = dest, .src = src, .idx = idx } });
        return dest;
    }

    pub fn emitCall(self: *IRFunction, dest: Temp, name: []const u8, args: std.ArrayList(Temp)) Temp {
        self.appendInstruction(.{ .call = .{ .dest = dest, .name = name, .args = args } });
        return dest;
    }

    pub fn emitAdd(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .add = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitSub(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .sub = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitMul(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .mul = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitDiv(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .div = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpEq(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_eq = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpNe(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_ne = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpGt(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_gt = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpLt(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_lt = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpGe(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_ge = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitCmpLe(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .cmp_le = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitAndAnd(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .andand = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitOrOr(self: *IRFunction, dest: Temp, left: Temp, right: Temp) Temp {
        self.appendInstruction(.{ .oror = .{ .dest = dest, .left = left, .right = right } });
        return dest;
    }

    pub fn emitBranch(self: *IRFunction, block_id: usize) void {
        self.appendInstruction(.{ .branch = block_id });
    }

    pub fn emitBranchIf(self: *IRFunction, condition: Temp, true_block_id: usize, false_block_id: usize) void {
        self.appendInstruction(.{ .branch_if = .{ .condition = condition, .true_block = true_block_id, .false_block = false_block_id } });
    }

    pub fn emitReturn(self: *IRFunction, expression: *Expression) void {
        self.appendInstruction(.{ .ret = self.emitExpression(expression) });
    }

    pub fn emitVarDeclare(self: *IRFunction, var_declare: VarDeclare) void {
        self.emitAlloca(var_declare.name, var_declare.type);
        _ = self.emitStore(var_declare.name, self.emitExpression(var_declare.value));
    }

    pub fn emitVarAssign(self: *IRFunction, var_assign: VarAssign) void {
        _ = self.emitStore(var_assign.name, self.emitExpression(var_assign.value));
    }

    pub fn emitExpression(self: *IRFunction, expression: *Expression) Temp {
        switch (expression.kind) {
            .number_literal => |n| return self.emitConst(self.newTemp(expression.ty), n),
            .identifier => |i| {
                var is_param = false;
                var param_idx: usize = std.math.maxInt(usize);

                for (self.signature.parameters.items, 0..) |param, idx| {
                    if (std.mem.eql(u8, i, param.name)) {
                        is_param = true;
                        param_idx = idx;
                        break;
                    }
                }

                if (is_param) return self.emitParam(self.newTemp(expression.ty), i, param_idx);
                return self.emitLoad(self.newTemp(expression.ty), i);
            },
            .fn_call => |f| {
                var tmps: std.ArrayList(Temp) = .empty;
                for (f.args.items) |e| tmps.append(self.allocator, self.emitExpression(e)) catch unreachable;
                return self.emitCall(self.newTemp(expression.ty), f.name, tmps);
            },
            .binary => |b| {
                const left = self.emitExpression(b.left);
                const right = self.emitExpression(b.right);

                const dest = self.newTemp(expression.ty);

                return switch (b.operator) {
                    .Add => self.emitAdd(dest, left, right),
                    .Subtract => self.emitSub(dest, left, right),
                    .Multiply => self.emitMul(dest, left, right),
                    .Divide => self.emitDiv(dest, left, right),
                    .Equal => self.emitCmpEq(dest, left, right),
                    .NotEqual => self.emitCmpNe(dest, left, right),
                    .Greater => self.emitCmpGt(dest, left, right),
                    .Less => self.emitCmpLt(dest, left, right),
                    .GreaterEqual => self.emitCmpGe(dest, left, right),
                    .LessEqual => self.emitCmpLe(dest, left, right),
                    .OrOr => self.emitOrOr(dest, left, right),
                    .AndAnd => self.emitAndAnd(dest, left, right),
                };
            },
        }
    }

    pub fn emitIf(self: *IRFunction, if_else: IfElse) void {
        const start_block = self.current_block;

        const if_block = self.appendBlock();
        self.emitBranchIf(self.emitExpression(if_else.condition), if_block, 0);

        self.current_block = if_block;
        for (if_else.if_block.items) |statement| self.emitStatement(statement);

        const merge_block = self.appendBlock();
        self.emitBranch(merge_block);
        self.currentBlock().then(self.allocator, self.getBlock(merge_block));

        self.current_block = merge_block;
        self.getBlock(start_block).terminator(self.instruction_pool).branch_if.false_block = merge_block;

        self.getBlock(start_block).then(self.allocator, self.getBlock(if_block));
        self.getBlock(start_block).then(self.allocator, self.getBlock(merge_block));
    }

    pub fn emitIfElse(self: *IRFunction, if_else: IfElse) void {
        const start_block = self.current_block;

        const if_block = self.appendBlock();
        self.emitBranchIf(self.emitExpression(if_else.condition), if_block, 0);

        self.current_block = if_block;
        for (if_else.if_block.items) |statement| self.emitStatement(statement);
        self.emitBranch(0);

        const else_block = self.appendBlock();

        self.getBlock(start_block).terminator(self.instruction_pool).branch_if.false_block = else_block;
        self.current_block = else_block;
        for (if_else.else_block.items) |statement| self.emitStatement(statement);

        const merge_block = self.appendBlock();
        self.emitBranch(merge_block);

        self.currentBlock().then(self.allocator, self.getBlock(merge_block));

        self.current_block = merge_block;
        self.getBlock(else_block - 1).terminator(self.instruction_pool).branch = merge_block;
        self.getBlock(else_block - 1).then(self.allocator, self.getBlock(merge_block));

        self.getBlock(start_block).then(self.allocator, self.getBlock(if_block));
        self.getBlock(start_block).then(self.allocator, self.getBlock(else_block));
    }

    pub fn emitConditional(self: *IRFunction, conditional: IfElse) void {
        if (conditional.else_block.items.len == 0) return self.emitIf(conditional);
        self.emitIfElse(conditional);
    }

    pub fn emitWhile(self: *IRFunction, loop_while: LoopWhile) void {
        const start_block = self.current_block;

        const cond_block = self.appendBlock();
        const body_block = self.appendBlock();

        self.emitBranch(cond_block);

        self.current_block = cond_block;
        self.emitBranchIf(self.emitExpression(loop_while.condition), body_block, 0);

        self.current_block = body_block;
        for (loop_while.body.items) |statement| self.emitStatement(statement);
        self.currentBlock().then(self.allocator, self.getBlock(cond_block));
        self.emitBranch(cond_block);

        const merge_block = self.appendBlock();
        self.getBlock(cond_block).terminator(self.instruction_pool).branch_if.false_block = merge_block;
        self.current_block = merge_block;

        self.getBlock(start_block).then(self.allocator, self.getBlock(cond_block));
        self.getBlock(cond_block).then(self.allocator, self.getBlock(body_block));
        self.getBlock(cond_block).then(self.allocator, self.getBlock(merge_block));
    }

    pub fn emitStatement(self: *IRFunction, statement: Statement) void {
        switch (statement) {
            .@"return" => |s| self.emitReturn(s),
            .expression => |s| _ = self.emitExpression(s),
            .var_declare => |s| self.emitVarDeclare(s),
            .var_assign => |s| self.emitVarAssign(s),
            .if_else => |s| self.emitConditional(s),
            .loop_while => |s| self.emitWhile(s),
        }
    }

    pub fn emit(self: *IRFunction, statements: []Statement) void {
        _ = self.appendBlock();
        for (statements) |statement| self.emitStatement(statement);
    }

    pub fn log(self: *IRFunction) void {
        for (self.blocks.items) |block_id| {
            const block = self.getBlock(block_id);

            if (block.id > 0) std.debug.print("{s}.{}:\n", .{ self.signature.name, block.id });

            for (block.instructions.items) |inst_id| {
                var instruction = self.instruction_pool.items[inst_id];

                std.debug.print("  ", .{});

                switch (instruction) {
                    .ret => |i| std.debug.print("ret t{}", .{i.id}),
                    .@"const" => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = const {}", .{i.value});
                    },
                    .alloca => |*i| {
                        std.debug.print("{s} = alloca ", .{i.name});
                        i.ty.log();
                    },
                    .store => |i| std.debug.print("{s} = store t{}", .{ i.dest, i.src.id }),
                    .load => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = load {s}", .{i.src});
                    },
                    .param => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = param {s}", .{i.src});
                    },
                    .call => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = call {s} ", .{i.name});

                        for (i.args.items) |arg| {
                            std.debug.print("t{} ", .{arg.id});
                        }
                    },
                    .add => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = add t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .sub => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = sub t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .mul => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = mul t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .div => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = div t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .andand => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = and t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .oror => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp or t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_eq => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp eq t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_ne => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp ne t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_gt => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp gt t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_lt => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp lt t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_ge => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp ge t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .cmp_le => |*i| {
                        std.debug.print("t{}:", .{i.dest.id});
                        i.dest.ty.log();
                        std.debug.print(" = cmp le t{} t{}", .{ i.left.id, i.right.id });
                    },
                    .branch => |i| std.debug.print("br {s}.{}", .{ self.signature.name, i }),
                    .branch_if => |i| std.debug.print("br t{} {s}.{} {s}.{}", .{ i.condition.id, self.signature.name, i.true_block, self.signature.name, i.false_block }),
                    .dead => std.debug.print("(dead)", .{}),
                }

                std.debug.print("\n", .{});
            }
        }
    }

    pub fn dot(self: *IRFunction, path: []const u8) void {
        var file = std.fs.cwd().createFile(path, .{}) catch unreachable;
        defer file.close();

        file.writeAll("digraph {\n") catch unreachable;

        for (self.blocks.items) |block_id| {
            const block = self.getBlock(block_id);

            const buf = std.fmt.allocPrint(self.allocator, "  {s}_{} [label=\"{s}.{}\"]\n", .{ self.signature.name, block.id, self.signature.name, block.id }) catch unreachable;
            defer self.allocator.free(buf);

            file.writeAll(buf) catch unreachable;
        }

        for (self.blocks.items) |block_id| {
            const block = self.getBlock(block_id);

            for (block.successors.items) |succ| {
                const buf = std.fmt.allocPrint(self.allocator, "  {s}_{} -> {s}_{}\n", .{ self.signature.name, block.id, self.signature.name, succ }) catch unreachable;
                defer self.allocator.free(buf);

                file.writeAll(buf) catch unreachable;
            }
        }

        file.writeAll("}") catch unreachable;
    }
};

pub const IRModule = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(IRFunction) = .empty,

    pub fn init(allocator: std.mem.Allocator) IRModule {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *IRModule) void {
        for (self.functions.items) |*func| func.deinit();
        self.functions.deinit(self.allocator);
    }

    pub fn emit(self: *IRModule, functions: []FnDeclare) void {
        for (functions) |func| {
            var function = IRFunction.init(self.allocator, func.signature);
            function.emit(func.body.items);
            self.functions.append(self.allocator, function) catch unreachable;
        }
    }

    pub fn log(self: *IRModule) void {
        for (self.functions.items) |*func| {
            std.debug.print("\n{s}:\n", .{func.signature.name});
            func.log();
        }
    }
};
