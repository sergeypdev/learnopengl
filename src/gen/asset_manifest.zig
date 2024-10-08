pub const manifest = @import("asset_manifest_gen");

pub const Scenes = manifest.Scenes;
pub const Meshes = manifest.Meshes;
pub const Shaders = manifest.Shaders;
pub const ShaderPrograms = manifest.ShaderPrograms;
pub const Textures = manifest.Textures;
pub const Materials = manifest.Materials;

pub fn getPath(asset_id: u64) []const u8 {
    manifest.init();
    if (asset_id == 0) return "";

    return manifest.asset_paths.get(asset_id) orelse "";
}

pub fn getAssetByPath(path: []const u8) u32 {
    return manifest.asset_path_to_asset_id.get(path) orelse 0;
}
