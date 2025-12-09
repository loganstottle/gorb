const std = @import("std");

const ir = @import("../ir.zig");

const ControlFlowGraph = ir.ControlFlowGraph;

const UNDEFINED = std.math.maxInt(usize);

pub const DomTree = struct {
    allocator: std.mem.Allocator,
    cfg: *ControlFlowGraph,
    idom: std.ArrayList(usize) = .empty,
    index: std.ArrayList(usize) = .empty,
    vertex: std.ArrayList(usize) = .empty,
    visited: std.ArrayList(bool) = .empty,
    cur: usize = 0,

    pub fn init(allocator: std.mem.Allocator, cfg: *ControlFlowGraph) DomTree {
        return .{ .allocator = allocator, .cfg = cfg };
    }

    pub fn number_postorder(self: *DomTree, bb: usize) void {
        if (self.visited.items[bb]) return;
        self.visited.items[bb] = true;

        for (self.cfg.blocks.items[bb].successors.items) |succ|
            self.number_postorder(succ);

        self.index.items[bb] = self.cur;
        self.vertex.items[self.cur] = bb;

        self.cur += 1;
    }

    pub fn intersect(self: *DomTree, b1: usize, b2: usize) usize {
        var f1 = b1;
        var f2 = b2;

        while (f1 != f2) {
            while (self.index.items[f1] < self.index.items[f2]) f1 = self.idom.items[f1];
            while (self.index.items[f2] < self.index.items[f1]) f2 = self.idom.items[f2];
        }

        return f1;
    }

    pub fn calculate(self: *DomTree) void {
        const n = self.cfg.blocks.items.len;

        self.visited.appendNTimes(self.allocator, false, n) catch unreachable;
        self.idom.appendNTimes(self.allocator, UNDEFINED, n) catch unreachable;
        self.index.appendNTimes(self.allocator, 0, n) catch unreachable;
        self.vertex.appendNTimes(self.allocator, 0, n) catch unreachable;

        self.number_postorder(0);

        self.idom.items[0] = 0;

        var changed = true;
        while (changed) {
            changed = false;

            var i: usize = self.cur - 2;
            while (true) {
                const b = self.vertex.items[i];

                var idom: usize = UNDEFINED;
                for (self.cfg.blocks.items[b].predecessors.items) |p| {
                    if (self.idom.items[p] != UNDEFINED) {
                        idom = p;
                        break;
                    }
                }

                for (self.cfg.blocks.items[b].predecessors.items) |p| {
                    if (p == idom or self.idom.items[p] == UNDEFINED) continue;
                    idom = self.intersect(idom, p);
                }

                if (self.idom.items[b] != idom) {
                    self.idom.items[b] = idom;
                    changed = true;
                }

                if (i == 0) break;
                i -= 1;
            }
        }

        for (0..n) |j| std.debug.print("idom(bb{}) = bb{}\n", .{ j, self.idom.items[j] });
    }
};
