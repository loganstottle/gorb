const std = @import("std");

const CFG = @import("../ir.zig").IRFunction;
const number_postorder = @import("../../util/traverse.zig").number_postorder;

const UNDEFINED = std.math.maxInt(usize);

const DEBUG = false;

pub const DomTree = struct {
    allocator: std.mem.Allocator,
    cfg: *CFG,
    idom: std.AutoHashMap(usize, usize),
    index: std.AutoHashMap(usize, usize),
    vertex: std.ArrayList(usize) = .empty,

    pub fn init(allocator: std.mem.Allocator, cfg: *CFG) DomTree {
        return .{
            .allocator = allocator,
            .cfg = cfg,
            .index = std.AutoHashMap(usize, usize).init(allocator),
            .idom = std.AutoHashMap(usize, usize).init(allocator),
        };
    }

    pub fn intersect(self: *DomTree, b1: usize, b2: usize) usize {
        var f1 = b1;
        var f2 = b2;

        while (f1 != f2) {
            while (self.index.get(f1).? < self.index.get(f2).?) f1 = self.idom.get(f1).?;
            while (self.index.get(f2).? < self.index.get(f1).?) f2 = self.idom.get(f2).?;
        }

        return f1;
    }

    pub fn calculate(self: *DomTree) void {
        const n = self.cfg.blocks.items.len;

        self.vertex.appendNTimes(self.allocator, 0, n) catch unreachable;
        number_postorder(self.cfg, &self.vertex, &self.index);

        // std.debug.print("{} {}\n", .{ self.index, self.vertex });

        self.idom.put(0, 0) catch unreachable;

        var changed = true;
        while (changed) {
            changed = false;

            var i: usize = n - 2;
            while (true) {
                const b = self.vertex.items[i];

                var idom: usize = UNDEFINED;
                for (self.cfg.getBlock(b).predecessors.items) |p| {
                    if (self.idom.get(p).? != UNDEFINED) {
                        idom = p;
                        break;
                    }
                }

                for (self.cfg.getBlock(b).predecessors.items) |p| {
                    if (p == idom or self.idom.get(p).? == UNDEFINED) continue;
                    idom = self.intersect(idom, p);
                }

                if (self.idom.get(b) == null or self.idom.get(b).? != idom) {
                    self.idom.put(b, idom) catch unreachable;
                    changed = true;
                }

                if (i == 0) break;
                i -= 1;
            }
        }

        // for (0..n) |j| std.debug.print("idom(bb{}) = bb{}\n", .{ j, self.idom.get(j).? });
    }
};
