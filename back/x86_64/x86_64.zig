const std = @import("std");

const parse = @import("../../front/parse.zig");

const Type = parse.Type;
const FnSignature = parse.FnSignature;

const IR = @import("../../mid/ir.zig");
const number_postorder = @import("../../util/traverse.zig").number_postorder;

const CFG = IR.ControlFlowGraph;
const IRModule = IR.IRModule;
const IRFunction = IR.IRFunction;
const IRInstruction = IR.Instruction;
const IRBlock = IR.BasicBlock;

const Imm = struct {
    value: i64,

    pub fn log(self: Imm) void {
        std.debug.print("{}", .{self.value});
    }
};

const Reg = union(enum) {
    virtual: usize,

    // general purpose
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    // reserved
    RSP,
    RBP,
    RIP,

    pub fn log(self: Reg) void {
        switch (self) {
            .RAX => std.debug.print("rax", .{}),
            .RBX => std.debug.print("rbx", .{}),
            .RCX => std.debug.print("rcx", .{}),
            .RDX => std.debug.print("rdx", .{}),
            .RSI => std.debug.print("rsi", .{}),
            .RDI => std.debug.print("rdi", .{}),
            .R8 => std.debug.print("r8", .{}),
            .R9 => std.debug.print("r9", .{}),
            .R10 => std.debug.print("r10", .{}),
            .R11 => std.debug.print("r11", .{}),
            .R12 => std.debug.print("r12", .{}),
            .R13 => std.debug.print("r13", .{}),
            .R14 => std.debug.print("r14", .{}),
            .R15 => std.debug.print("r15", .{}),
            .RSP => std.debug.print("rsp", .{}),
            .RBP => std.debug.print("rbp", .{}),
            .RIP => std.debug.print("rip", .{}),
            .virtual => |v| std.debug.print("v{}", .{v}),
        }
    }
};

const Addr = struct {
    base: ?Reg,
    index: ?Reg,
    scale: u8, // 1, 2, 4, 8
    offset: i64,
};

const Mem = union(enum) {
    stack_slot: usize,
    mem: struct {
        addr: *Addr,
        size: u8,
    },

    pub fn log(self: Mem) void {
        switch (self) {
            .stack_slot => |s| std.debug.print("frame_index({})", .{s}),
            .mem => std.debug.print("MEM(TODO)", .{}),
        }
    }
};

const rr = struct {
    dest: Reg,
    src: Reg,

    pub fn log(self: rr) void {
        self.dest.log();
        std.debug.print(", ", .{});
        self.src.log();
    }
};

const ri = struct {
    dest: Reg,
    src: Imm,

    pub fn log(self: ri) void {
        self.dest.log();
        std.debug.print(", ", .{});
        self.src.log();
    }
};

const rm = struct {
    dest: Reg,
    src: Mem,

    pub fn log(self: rm) void {
        self.dest.log();
        std.debug.print(", ", .{});
        self.src.log();
    }
};

const mr = struct {
    dest: Mem,
    src: Reg,

    pub fn log(self: mr) void {
        self.dest.log();
        std.debug.print(", ", .{});
        self.src.log();
    }
};

const x86Instruction = union(enum) {
    MOVrr: rr,
    MOVri: ri,
    MOVrm: rm,
    MOVmr: mr,
    MOVZXrr: mr,
    ADDrr: rr,
    SUBrr: rr,
    IMULrr: rr,
    IDIVr: Reg,
    CALLl: []const u8,
    JMPl: usize,
    JZl: usize,
    CMPrr: rr,
    SETEr: Reg,
    SETNEr: Reg,
    SETGTr: Reg,
    SETLTr: Reg,
    SETGEr: Reg,
    SETLEr: Reg,
    TESTrr: rr,
};

pub const Block = struct {
    id: usize,
    instructions: std.ArrayList(x86Instruction) = .empty,
};

pub const StackFrame = struct {
    cur_slot: usize,
    vars: std.StringHashMap(usize),
};

//abi

const RetReg = .RAX;

pub const IntPtrArgs = [6]Reg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };

pub const LLIRFunction = struct {
    frame: StackFrame,
    old_func: *IRFunction,
    allocator: std.mem.Allocator,
    signature: FnSignature,
    blocks: std.ArrayList(Block) = .empty,
    block_indexes: std.AutoHashMap(usize, usize), // ir block id -> llir block index
    current_block: usize = 0,

    pub fn init(ir_func: *IRFunction) LLIRFunction {
        return .{
            .old_func = ir_func,
            .allocator = ir_func.allocator,
            .signature = ir_func.signature,
            .block_indexes = std.AutoHashMap(usize, usize).init(ir_func.allocator),
            .frame = .{
                .cur_slot = 0,
                .vars = std.StringHashMap(usize).init(ir_func.allocator),
            },
        };
    }

    pub fn getCurrentBlock(self: *LLIRFunction) *Block {
        return &self.blocks.items[self.current_block];
    }

    pub fn appendInstruction(self: *LLIRFunction, instr: x86Instruction) void {
        self.getCurrentBlock().instructions.append(self.allocator, instr) catch unreachable;
    }

    pub fn emitMOVrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .MOVrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitMOVri(self: *LLIRFunction, dest: Reg, src: Imm) void {
        self.appendInstruction(.{ .MOVri = .{ .dest = dest, .src = src } });
    }

    pub fn emitMOVrm(self: *LLIRFunction, dest: Reg, src: Mem) void {
        self.appendInstruction(.{ .MOVrm = .{ .dest = dest, .src = src } });
    }

    pub fn emitMOVmr(self: *LLIRFunction, dest: Mem, src: Reg) void {
        self.appendInstruction(.{ .MOVmr = .{ .dest = dest, .src = src } });
    }

    pub fn emitCALLl(self: *LLIRFunction, name: []const u8) void {
        self.appendInstruction(.{ .CALLl = name });
    }

    pub fn emitJMPl(self: *LLIRFunction, target: usize) void {
        self.appendInstruction(.{ .JMPl = target });
    }

    pub fn emitJZl(self: *LLIRFunction, target: usize) void {
        self.appendInstruction(.{ .JZl = target });
    }

    pub fn emitADDrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .ADDrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitSUBrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .SUBrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitIMULrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .IMULrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitIDIVrr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .IDIVrr = .{ .src = src } });
    }

    pub fn emitCMPrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .CMPrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitSETEr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETEr = src });
    }

    pub fn emitSETNEr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETNEr = .{ .src = src } });
    }

    pub fn emitSETGTr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETGTr = .{ .src = src } });
    }

    pub fn emitSETLTr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETLTr = .{ .src = src } });
    }

    pub fn emitSETGEr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETGEr = .{ .src = src } });
    }

    pub fn emitSETLEr(self: *LLIRFunction, src: Reg) void {
        self.appendInstruction(.{ .SETLEr = .{ .src = src } });
    }

    pub fn emitMOVZXrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .MOVZXrr = .{ .dest = dest, .src = src } });
    }

    pub fn emitTESTrr(self: *LLIRFunction, dest: Reg, src: Reg) void {
        self.appendInstruction(.{ .TESTrr = .{ .dest = dest, .src = src } });
    }

    pub fn lowerRet(self: *LLIRFunction, ret: IR.Temp) void {
        self.emitMOVrr(RetReg, .{ .virtual = ret.id });
    }

    pub fn lowerConst(self: *LLIRFunction, constt: IR.Const) void {
        self.emitMOVri(.{ .virtual = constt.dest.id }, .{ .value = constt.value });
    }

    pub fn lowerAlloca(self: *LLIRFunction, alloca: IR.Alloca) void {
        self.frame.vars.put(alloca.name, self.frame.cur_slot) catch unreachable;
        self.frame.cur_slot += 1;
    }

    pub fn lowerStore(self: *LLIRFunction, store: IR.Store) void {
        const stack_slot = self.frame.vars.get(store.dest).?;
        self.emitMOVmr(.{ .stack_slot = stack_slot }, .{ .virtual = store.src.id });
    }

    pub fn lowerLoad(self: *LLIRFunction, load: IR.Load) void {
        const stack_slot = self.frame.vars.get(load.src);
        self.emitMOVrm(.{ .virtual = load.dest.id }, .{ .stack_slot = stack_slot.? });
    }

    pub fn lowerParam(self: *LLIRFunction, param: IR.Param) void {
        std.debug.assert(param.idx < IntPtrArgs.len);
        self.emitMOVrr(.{ .virtual = param.dest.id }, IntPtrArgs[param.idx]);
    }

    pub fn lowerCall(self: *LLIRFunction, call: IR.Call) void {
        // todo
        std.debug.assert(call.args.items.len <= IntPtrArgs.len);

        for (call.args.items, 0..) |arg, idx|
            self.emitMOVrr(IntPtrArgs[idx], .{ .virtual = arg.id });

        self.emitCALLl(call.name);
        self.emitMOVrr(.{ .virtual = call.dest.id }, RetReg);
    }

    pub fn lowerAdd(self: *LLIRFunction, add: IR.BinaryOp) void {
        self.emitMOVrr(.{ .virtual = add.dest.id }, .{ .virtual = add.left.id });
        self.emitADDrr(.{ .virtual = add.dest.id }, .{ .virtual = add.right.id });
    }

    pub fn lowerSub(self: *LLIRFunction, sub: IR.BinaryOp) void {
        self.emitMOVrr(.{ .virtual = sub.dest.id }, .{ .virtual = sub.left.id });
        self.emitSUBrr(.{ .virtual = sub.dest.id }, .{ .virtual = sub.right.id });
    }

    pub fn lowerMul(self: *LLIRFunction, mul: IR.BinaryOp) void {
        self.emitMOVrr(.{ .virtual = mul.dest.id }, .{ .virtual = mul.left.id });
        self.emitIMULrr(.{ .virtual = mul.dest.id }, .{ .virtual = mul.right.id });
    }

    pub fn lowerDiv(self: *LLIRFunction, div: IR.BinaryOp) void {
        _ = self;
        _ = div;
    }

    pub fn lowerAndAnd(self: *LLIRFunction, andand: IR.BinaryOp) void {
        _ = self;
        _ = andand;
    }

    pub fn lowerOrOr(self: *LLIRFunction, oror: IR.BinaryOp) void {
        _ = self;
        _ = oror;
    }

    // signed: gt, lt, ge, le, e, ne

    pub fn lowerCmpEq(self: *LLIRFunction, cmp_eq: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_eq.left.id }, .{ .virtual = cmp_eq.right.id });
        self.emitSETEr(.{ .virtual = cmp_eq.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerCmpNe(self: *LLIRFunction, cmp_ne: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_ne.left.id }, .{ .virtual = cmp_ne.right.id });
        self.emitSETEr(.{ .virtual = cmp_ne.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerCmpGt(self: *LLIRFunction, cmp_gt: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_gt.left.id }, .{ .virtual = cmp_gt.right.id });
        self.emitSETEr(.{ .virtual = cmp_gt.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerCmpLt(self: *LLIRFunction, cmp_lt: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_lt.left.id }, .{ .virtual = cmp_lt.right.id });
        self.emitSETEr(.{ .virtual = cmp_lt.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerCmpGe(self: *LLIRFunction, cmp_ge: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_ge.left.id }, .{ .virtual = cmp_ge.right.id });
        self.emitSETEr(.{ .virtual = cmp_ge.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerCmpLe(self: *LLIRFunction, cmp_le: IR.BinaryOp) void {
        self.emitCMPrr(.{ .virtual = cmp_le.left.id }, .{ .virtual = cmp_le.right.id });
        self.emitSETEr(.{ .virtual = cmp_le.dest.id });
        //self.emitMOVZXrr();
    }

    pub fn lowerBranch(self: *LLIRFunction, branch: usize) void {
        self.emitJMPl(branch);
    }

    pub fn lowerBranchIf(self: *LLIRFunction, branch_if: IR.BranchIf) void {
        self.emitTESTrr(.{ .virtual = branch_if.condition.id }, .{ .virtual = branch_if.condition.id });
        self.emitJZl(branch_if.false_block);
        self.emitJMPl(branch_if.true_block);
    }

    pub fn lower(self: *LLIRFunction) void {
        for (self.old_func.blocks.items) |block_id| {
            self.block_indexes.put(block_id, self.blocks.items.len) catch unreachable;
            self.blocks.append(self.allocator, .{ .id = block_id }) catch unreachable;
        }

        var po: std.ArrayList(usize) = .empty;
        var rpo: std.ArrayList(usize) = .empty;

        po.appendNTimes(self.allocator, 0, self.old_func.blocks.items.len) catch unreachable;
        number_postorder(self.old_func, &po, null);

        var i: usize = self.old_func.blocks.items.len - 1;
        while (true) {
            rpo.append(self.allocator, po.items[i]) catch unreachable;

            if (i == 0) break;
            i -= 1;
        }

        for (rpo.items) |block_id| {
            std.debug.print("block {}\n", .{block_id});
            self.current_block = self.block_indexes.get(block_id).?;

            for (self.old_func.getBlock(block_id).instructions.items) |instr_id| {
                const ir_instr = self.old_func.instruction_pool.items[instr_id];

                //std.debug.print("instruction: {}\n", .{ir_instr});

                switch (ir_instr) {
                    .ret => |inst| self.lowerRet(inst),
                    .@"const" => |inst| self.lowerConst(inst),
                    .alloca => |inst| self.lowerAlloca(inst),
                    .store => |inst| self.lowerStore(inst),
                    .load => |inst| self.lowerLoad(inst),
                    .param => |inst| self.lowerParam(inst),
                    .call => |inst| self.lowerCall(inst),
                    .add => |inst| self.lowerAdd(inst),
                    .sub => |inst| self.lowerSub(inst),
                    .mul => |inst| self.lowerMul(inst),
                    .div => |inst| self.lowerDiv(inst),
                    .andand => |inst| self.lowerAndAnd(inst),
                    .oror => |inst| self.lowerOrOr(inst),
                    .cmp_eq => |inst| self.lowerCmpEq(inst),
                    .cmp_ne => |inst| self.lowerCmpNe(inst),
                    .cmp_gt => |inst| self.lowerCmpGt(inst),
                    .cmp_lt => |inst| self.lowerCmpLt(inst),
                    .cmp_ge => |inst| self.lowerCmpGe(inst),
                    .cmp_le => |inst| self.lowerCmpLe(inst),
                    .branch => |inst| self.lowerBranch(inst),
                    .branch_if => |inst| self.lowerBranchIf(inst),
                    .dead => unreachable,
                }
            }
        }
    }

    pub fn log(self: *LLIRFunction) void {
        for (self.blocks.items) |block| {
            if (block.id > 0) std.debug.print("{s}.{}:\n", .{ self.signature.name, block.id });

            for (block.instructions.items) |inst| {
                std.debug.print("  ", .{});

                switch (inst) {
                    .MOVrr => |i| {
                        std.debug.print("mov ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .MOVri => |i| {
                        std.debug.print("mov ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .MOVrm => |i| {
                        std.debug.print("mov ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .MOVmr => |i| {
                        std.debug.print("mov ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .MOVZXrr => |i| {
                        std.debug.print("movzx ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .ADDrr => |i| {
                        std.debug.print("add ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SUBrr => |i| {
                        std.debug.print("sub ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .IMULrr => |i| {
                        std.debug.print("imul ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .IDIVr => |i| {
                        std.debug.print("idiv ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .CALLl => |i| std.debug.print("call {s}\n", .{i}),
                    .JMPl => |i| std.debug.print("jmp {s}.{}\n", .{ self.signature.name, i }),
                    .JZl => |i| std.debug.print("jz {s}.{}\n", .{ self.signature.name, i }),
                    .CMPrr => |i| {
                        std.debug.print("cmp ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETEr => |i| {
                        std.debug.print("sete ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETNEr => |i| {
                        std.debug.print("setne ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETGTr => |i| {
                        std.debug.print("setgt ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETLTr => |i| {
                        std.debug.print("setlt ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETGEr => |i| {
                        std.debug.print("setge ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .SETLEr => |i| {
                        std.debug.print("setle ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                    .TESTrr => |i| {
                        std.debug.print("test ", .{});
                        i.log();
                        std.debug.print("\n", .{});
                    },
                }
            }

            std.debug.print("\n", .{});
        }
    }
};

pub const LLIRModule = struct {
    allocator: std.mem.Allocator,
    old_ir: *IRModule,
    functions: std.ArrayList(LLIRFunction) = .empty,

    pub fn init(old_ir: *IRModule) LLIRModule {
        return .{ .allocator = old_ir.allocator, .old_ir = old_ir };
    }

    pub fn lower(self: *LLIRModule) void {
        for (self.old_ir.functions.items) |*func| {
            var new_func = LLIRFunction.init(func);
            new_func.lower();
            self.functions.append(self.allocator, new_func) catch unreachable;
        }
    }

    pub fn log(self: *LLIRModule) void {
        for (self.functions.items) |*func| {
            std.debug.print("\n{s}:\n", .{func.signature.name});
            func.log();
        }
    }
};
