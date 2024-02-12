pub const manifest = @import("asset_manifest_gen");

pub const Meshes = manifest.Meshes;
pub const Shaders = manifest.Shaders;
pub const ShaderPrograms = manifest.ShaderPrograms;

pub fn getPath(asset_id: u32) []const u8 {
    if (asset_id == 0) return "";

    return manifest.asset_paths[asset_id - 1];
}

pub fn getAssetByPath(path: []const u8) u32 {
    return manifest.asset_path_to_asset_id.get(path) orelse 0;
}
