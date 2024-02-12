// Generated file, do not edit manually!

const std = @import("std");
const Handle = @import("assets").Handle;

pub const Meshes = struct {
    pub const bunny = Handle.Mesh{ .id = 1 };
    pub const plane = Handle.Mesh{ .id = 2 };
    pub const sphere = Handle.Mesh{ .id = 8 };
};

pub const Shaders = struct {
    pub const frag = Handle.Shader{ .id = 3 };
    pub const @"mesh.frag" = Handle.Shader{ .id = 4 };
    pub const @"mesh.vert" = Handle.Shader{ .id = 6 };
    pub const vert = Handle.Shader{ .id = 7 };
};

pub const ShaderPrograms = struct {
    pub const mesh = Handle.ShaderProgram{ .id = 5 };
};

pub const asset_paths = [_][]const u8{
    "assets\\bunny.mesh",
    "assets\\plane.mesh",
    "assets\\shaders\\frag.glsl",
    "assets\\shaders\\mesh.frag.glsl",
    "assets\\shaders\\mesh.prog",
    "assets\\shaders\\mesh.vert.glsl",
    "assets\\shaders\\vert.glsl",
    "assets\\sphere.mesh",
};

pub const asset_path_to_asset_id = std.ComptimeStringMap(u32, .{
    .{ "assets\\bunny.mesh", 1 },
    .{ "assets\\plane.mesh", 2 },
    .{ "assets\\shaders\\frag.glsl", 3 },
    .{ "assets\\shaders\\mesh.frag.glsl", 4 },
    .{ "assets\\shaders\\mesh.prog", 5 },
    .{ "assets\\shaders\\mesh.vert.glsl", 6 },
    .{ "assets\\shaders\\vert.glsl", 7 },
    .{ "assets\\sphere.mesh", 8 },
});

