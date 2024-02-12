// Generated file, do not edit manually!

const std = @import("std");
const Handle = @import("assets").Handle;

pub const Meshes = struct {
    pub const bunny = Handle.Mesh{ .id = 1 };
    pub const plane = Handle.Mesh{ .id = 2 };
    pub const sphere = Handle.Mesh{ .id = 5 };
};

pub const Shaders = struct {
    pub const mesh = Handle.Shader{ .id = 3 };
};

pub const ShaderPrograms = struct {
    pub const mesh = Handle.ShaderProgram{ .id = 4 };
};

pub const asset_paths = [_][]const u8{
    "assets\\bunny.mesh",
    "assets\\plane.mesh",
    "assets\\shaders\\mesh.glsl",
    "assets\\shaders\\mesh.prog",
    "assets\\sphere.mesh",
};

pub const asset_path_to_asset_id = std.ComptimeStringMap(u32, .{
    .{ "assets\\bunny.mesh", 1 },
    .{ "assets\\plane.mesh", 2 },
    .{ "assets\\shaders\\mesh.glsl", 3 },
    .{ "assets\\shaders\\mesh.prog", 4 },
    .{ "assets\\sphere.mesh", 5 },
});

