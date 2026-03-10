const std = @import("std");

const Type = @import("../../front/parse.zig").Type;
const IR = @import("../../mid/ir.zig");
const number_postorder = @import("../../util/traverse.zig").number_postorder;

const CFG = IR.ControlFlowGraph;
const IRModule = IR.IRModule;
const IRInstruction = IR.Instruction;

const Imm = struct {
    value: i64,
    ty: Type,
};

const Reg = union(enum) {
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

    VIRTUAL: u64,
};

const Addr = struct {
    base: ?Reg,
    index: ?Reg,
    scale: u8, // 1, 2, 4, 8
    offset: i64,
};

const Mem = struct {
    addr: *Addr,
    size: u8,
};

const Operand = union(enum) {
    imm: Imm,
    reg: Reg,
    addr: Addr,
    mem: Mem,
};

const x86Instruction = union(enum) {
    label: []const u8,

    MOVrr: struct { dest: Reg, src: Reg },
    ADDrr: struct { dest: Reg, src: Reg },
};

pub const LLIRModule = struct {
    allocator: std.mem.Allocator,
    old_ir: *IRModule,

    pub fn init(allocator: std.mem.Allocator, old_ir: *IRModule) LLIRModule {
        return LLIRModule{ .allocator = allocator, .old_ir = old_ir };
    }
};
