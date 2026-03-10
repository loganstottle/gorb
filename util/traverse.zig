const std = @import("std");

const ControlFlowGraph = @import("../mid/ir.zig").ControlFlowGraph;

pub fn number_postorder(cfg: *ControlFlowGraph, allocator: std.mem.Allocator, numbering: *std.ArrayList(usize), inverse_numbering: ?*std.ArrayList(usize)) void {
    var visited: std.ArrayList(bool) = .empty;
    visited.appendNTimes(allocator, false, cfg.blocks.items.len) catch unreachable;
    var cur: usize = 0;

    _number_postorder(cfg, 0, &visited, numbering, inverse_numbering, &cur);
}

fn _number_postorder(cfg: *ControlFlowGraph, bb: usize, visited: *std.ArrayList(bool), numbering: *std.ArrayList(usize), inverse_numbering: ?*std.ArrayList(usize), cur: *usize) void {
    if (visited.items[bb]) return;
    visited.items[bb] = true;

    for (cfg.blocks.items[bb].successors.items) |succ|
        _number_postorder(cfg, succ, visited, numbering, inverse_numbering, cur);

    numbering.items[bb] = cur.*;
    if (inverse_numbering) |inv| inv.items[cur.*] = bb;

    cur.* += 1;
}
