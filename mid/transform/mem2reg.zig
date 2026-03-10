const std = @import("std");

const ControlFlowGraph = @import("../ir.zig").ControlFlowGraph;

pub fn dominance_frontier(allocator: std.mem.Allocator, cfg: *ControlFlowGraph, idom: std.ArrayList(usize)) std.ArrayList(std.ArrayList(usize)) {
    var df: std.ArrayList(std.ArrayList(usize)) = .empty;

    var i: usize = 0;
    while (i < cfg.blocks.items.len) : (i += 1) df.append(allocator, .empty) catch unreachable;

    for (cfg.blocks.items) |bb| {
        if (bb.predecessors.items.len < 2) continue;

        for (bb.predecessors.items) |pred| {
            var runner = pred;

            while (runner != idom.items[bb.id]) {
                df.items[runner].append(allocator, bb.id) catch unreachable;
                runner = idom.items[runner];
            }
        }
    }

    return df;
}

pub fn mem2reg(allocator: std.mem.Allocator, cfg: *ControlFlowGraph, idoms: std.ArrayList(usize)) std.StringHashMap(std.AutoHashMap(usize, void)) {
    const df = dominance_frontier(allocator, cfg, idoms);

    var result = std.StringHashMap(std.AutoHashMap(usize, void)).init(allocator);

    for (cfg.vars.items) |v| {
        var worklist: std.ArrayList(usize) = .empty;

        for (cfg.blocks.items) |bb| {
            for (bb.instructions.items) |instr| {
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
