const std = @import("std");

const CFG = @import("../ir.zig").ControlFlowGraph;
const number_postorder = @import("../../util/traverse.zig").number_postorder;

const UNDEFINED = std.math.maxInt(usize);

pub const DomTree = struct {
    allocator: std.mem.Allocator,
    cfg: *CFG,
    idom: std.ArrayList(usize) = .empty,
    index: std.ArrayList(usize) = .empty,
    vertex: std.ArrayList(usize) = .empty,

    pub fn init(allocator: std.mem.Allocator, cfg: *CFG) DomTree {
        return .{ .allocator = allocator, .cfg = cfg };
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

        self.idom.appendNTimes(self.allocator, UNDEFINED, n) catch unreachable;
        self.index.appendNTimes(self.allocator, 0, n) catch unreachable;
        self.vertex.appendNTimes(self.allocator, 0, n) catch unreachable;

        number_postorder(self.cfg, self.allocator, &self.index, &self.vertex);
        std.debug.print("{} {}\n", .{ self.index, self.vertex });

        self.idom.items[0] = 0;

        var changed = true;
        while (changed) {
            changed = false;

            var i: usize = n - 2;
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
