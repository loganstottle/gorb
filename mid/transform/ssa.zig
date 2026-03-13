const std = @import("std");

const IRFunction = @import("../ir.zig").IRFunction;

// todo: turn df into a hash map

pub fn dominance_frontier(allocator: std.mem.Allocator, cfg: *IRFunction, idom: std.AutoHashMap(usize, usize)) std.ArrayList(std.ArrayList(usize)) {
    var df: std.ArrayList(std.ArrayList(usize)) = .empty;

    var i: usize = 0;
    while (i < cfg.blocks.items.len) : (i += 1) df.append(allocator, .empty) catch unreachable;

    for (cfg.blocks.items) |block_id| {
        const block = cfg.getBlock(block_id);

        if (block.predecessors.items.len < 2) continue;

        for (block.predecessors.items) |pred| {
            var runner = pred;

            while (runner != idom.get(block_id).?) {
                df.items[runner].append(allocator, block_id) catch unreachable;
                runner = idom.get(runner).?;
            }
        }
    }

    return df;
}

pub fn mem2reg(allocator: std.mem.Allocator, cfg: *IRFunction, idoms: std.AutoHashMap(usize, usize)) std.StringHashMap(std.AutoHashMap(usize, void)) {
    const df = dominance_frontier(allocator, cfg, idoms);

    for (df.items, 0..df.items.len) |arr, idx| {
        std.debug.print("df[{any}]: {any}\n", .{ idx, arr.items });
    }

    var result = std.StringHashMap(std.AutoHashMap(usize, void)).init(allocator);

    for (cfg.vars.items) |v| {
        var worklist: std.ArrayList(usize) = .empty;

        for (cfg.blocks.items) |block_id| {
            const bb = cfg.getBlock(block_id);

            for (bb.instructions.items) |instr_id| {
                const instr = cfg.instruction_pool.items[instr_id];

                switch (instr) {
                    .store => |s| if (std.mem.eql(u8, s.dest, v)) worklist.append(allocator, bb.id) catch unreachable,
                    else => {},
                }
            }
        }

        const pre = worklist.clone(allocator) catch unreachable;

        var visited: std.ArrayList(bool) = .empty;
        visited.appendNTimes(allocator, false, cfg.blocks.items.len) catch unreachable;

        while (worklist.pop()) |bb| {
            for (df.items[bb].items) |f| {
                if (visited.items[f]) continue;
                visited.items[f] = true;

                if (result.get(v) == null) {
                    const map = std.AutoHashMap(usize, void).init(allocator);
                    result.put(v, map) catch unreachable;
                }

                std.debug.print("variable {s} needs a phi node coming from block {} to block {}\n", .{ v, bb, f });
                result.getPtr(v).?.put(f, {}) catch unreachable;

                var ok = true;
                for (pre.items) |b| {
                    if (b == f) ok = false;
                }

                if (ok) worklist.append(allocator, f) catch unreachable;
            }
        }
    }

    // todo:
    //   variable renaming
    //   phi insertion

    return result;
}
