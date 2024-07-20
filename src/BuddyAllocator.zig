const std = @import("std");

pub const BuddyAllocator = @This();

pub const Alloc = struct {
    offset: u32 = 0,
    depth: u6 = 0,
};

const BlockState = enum(u2) {
    Free,
    /// This block is actually allocated
    Allocated,
    /// When small block is allocated out of a large block, the large block and all its parents are split
    Split,
};

const BlockStateArray = std.PackedIntSlice(u2);

allocator: std.mem.Allocator,
depth: u6,
min_block_size: usize,
states: []BlockState,

/// min_block_size - is the size of leaf blocks at the bottom of the binary tree
/// depth - depth of the binary tree
///
/// Largest block in the buffer will be: 2^(log2(min_block_size) + depth)
/// or more simply put (min_block_size << depth)
///
/// e.g.
/// min_block_size = 64
/// depth = 2
///          256
///        /     \
///       /       \
///      /         \
///    128         128
///   /   \       /   \
///  /     \     /     \
/// 64     64   64     64
pub fn init(allocator: std.mem.Allocator, min_block_size: usize, depth: u6) !BuddyAllocator {
    std.debug.assert(min_block_size > 0 and (min_block_size & (min_block_size - 1)) == 0);

    const size = min_block_size << depth;
    const states_len = (size * 2) - 1;
    const states = try allocator.alloc(BlockState, states_len);
    @memset(states, BlockState.Free);

    return .{
        .allocator = allocator,
        .depth = depth,
        .min_block_size = min_block_size,
        .states = states,
    };
}

pub fn deinit(self: *BuddyAllocator) void {
    self.allocator.free(self.states);
}

/// Returns a memory address
pub fn alloc(self: *BuddyAllocator, size: usize) !Alloc {
    var ctx = SearchContext{};
    self.findBlock(size, 0, false, &ctx);

    if (ctx.found) {
        self.states[ctx.node] = BlockState.Allocated;

        var parent = @divFloor(@as(isize, @intCast(ctx.node)) - 1, 2);
        while (parent >= 0) : (parent = @divFloor(parent - 1, 2)) {
            self.states[@intCast(parent)] = BlockState.Split;
        }

        const depth = getNodeDepth(ctx.node);
        const block_size = self.getDepthSize(depth);

        // Index of the first node at this depth
        // All nodes at a single depth are laid out sequentially
        const depth_offset = (@as(usize, @intCast(1)) << depth) - 1;
        const idx = ctx.node - depth_offset;

        return .{ .offset = @intCast(idx * block_size), .depth = depth };
    }

    return error.OutOfMemory;
}

pub fn free(self: *BuddyAllocator, allocation: Alloc) void {
    if (allocation.depth == 0) {
        std.debug.assert(self.states[0] == BlockState.Allocated);
        self.states[0] = BlockState.Free;
    } else {
        const size = self.getDepthSize(@intCast(allocation.depth));
        const depth_offset = (@as(usize, @intCast(1)) << allocation.depth) - 1;

        const idx = @as(usize, @intCast(allocation.offset)) / size;
        const node = idx + depth_offset;

        std.debug.assert(self.states[node] == BlockState.Allocated);
        self.states[node] = BlockState.Free;

        // Merge
        {
            var parent = @divFloor(@as(isize, @intCast(node)) - 1, 2);
            while (parent >= 0) : (parent = @divFloor(parent - 1, 2)) {
                const parent_usize: usize = @intCast(parent);
                if (self.states[parent_usize * 2 + 1] == BlockState.Free and self.states[parent_usize * 2 + 2] == BlockState.Free) {
                    self.states[parent_usize] = BlockState.Free;
                }
            }
        }
    }
}

const SearchContext = struct {
    sort_key: usize = std.math.maxInt(usize),
    node: usize = 0,
    found: bool = false,
};

/// e.g.
/// min_block_size = 64
/// depth = 2
///          256
///        /     \
///       /       \
///      /         \
///    128         128
///   /   \       /   \
///  /     \     /     \
/// 64     64   64     64
fn findBlock(self: *BuddyAllocator, size: usize, node: usize, under_split: bool, ctx: *SearchContext) void {
    // Size of the block on the current level
    const depth = getNodeDepth(node);
    const depth_size = self.getDepthSize(depth);

    if (size <= depth_size) {
        const state: BlockState = self.states[node];

        if (state != BlockState.Allocated) {
            // Trying to find the best place for allocation
            // It has to be the smallest block that fits size
            // and preferrably it must be allocated from already split large blocks
            // to leave as many free large blocks as possible
            const sort_key = depth_size << (if (under_split) 0 else 1);

            if (state == BlockState.Free and (!ctx.found or sort_key < ctx.sort_key)) {
                ctx.sort_key = sort_key;
                ctx.node = node;
                ctx.found = true;
            }

            if (depth < self.depth) {
                findBlock(self, size, node * 2 + 1, state == BlockState.Split and node != 0, ctx);
                findBlock(self, size, node * 2 + 2, state == BlockState.Split and node != 0, ctx);
            }
        }
    }
}

/// Returns node's depth by index
fn getNodeDepth(node: usize) u6 {
    return @intCast(std.math.log2(node + 1));
}

/// Returns byte size of blocks at this depth
fn getDepthSize(self: *const BuddyAllocator, depth: u6) usize {
    return (@as(usize, 1) << (self.depth - depth)) * self.min_block_size;
}

pub fn getSize(self: *const BuddyAllocator) usize {
    return self.getDepthSize(0);
}

test "Buddy Allocator" {
    var buddy = try BuddyAllocator.init(std.testing.allocator, 64, 2);
    defer buddy.deinit();

    const al1 = try buddy.alloc(64);
    try std.testing.expectEqual(Alloc{ .offset = 0, .depth = 2 }, al1);

    const al2 = try buddy.alloc(65);
    try std.testing.expectEqual(Alloc{ .offset = 128, .depth = 1 }, al2);

    const al3 = try buddy.alloc(32);
    try std.testing.expectEqual(Alloc{ .offset = 64, .depth = 2 }, al3);

    buddy.free(al2);

    const al4 = try buddy.alloc(32);
    try std.testing.expectEqual(Alloc{ .offset = 128, .depth = 2 }, al4);

    const al5 = try buddy.alloc(32);
    try std.testing.expectEqual(Alloc{ .offset = 192, .depth = 2 }, al5);

    buddy.free(al1);
    buddy.free(al3);
    buddy.free(al5);

    const al6 = try buddy.alloc(32);
    try std.testing.expectEqual(Alloc{ .offset = 192, .depth = 2 }, al6);

    buddy.free(al4);
    buddy.free(al6);

    const al7 = try buddy.alloc(129);
    try std.testing.expectEqual(Alloc{ .offset = 0, .depth = 0 }, al7);

    buddy.free(al7);

    try std.testing.expect(std.mem.allEqual(BlockState, buddy.states, BlockState.Free));
}
