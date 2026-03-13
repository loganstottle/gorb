const std = @import("std");

const IRFunction = @import("../mid/ir.zig").IRFunction;

pub fn number_postorder(cfg: *IRFunction, dfs: *std.ArrayList(usize), inv_dfs: ?*std.AutoHashMap(usize, usize)) void {
    var visited = std.AutoHashMap(usize, bool).init(cfg.allocator);
    defer visited.deinit();

    var cur_idx: usize = 0;
    _number_postorder(cfg, dfs, inv_dfs, &visited, &cur_idx, cfg.blocks.items[0]);
}

pub fn _number_postorder(cfg: *IRFunction, dfs: *std.ArrayList(usize), inv_dfs: ?*std.AutoHashMap(usize, usize), visited: *std.AutoHashMap(usize, bool), cur_idx: *usize, block_id: usize) void {
    if (visited.get(block_id) != null) return;
    visited.put(block_id, true) catch unreachable;

    for (cfg.getBlock(block_id).successors.items) |succ_id|
        _number_postorder(cfg, dfs, inv_dfs, visited, cur_idx, succ_id);

    dfs.items[cur_idx.*] = block_id;
    if (inv_dfs) |inv| inv.put(block_id, cur_idx.*) catch unreachable;

    cur_idx.* += 1;
}
