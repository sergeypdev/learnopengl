// Generated file, do not edit manually!

const Handle = @import("Assets.zig").Handle;

pub const Meshes = struct {
    pub const bunny = Handle.Mesh{ .id = 1 };
};
pub const asset_paths = [_][]const u8{
    ".\\assets\\bunny.mesh",
};
pub fn getPath(asset_id: u32) []const u8 { return asset_paths[asset_id - 1]; }
