const std = @import("std");
const formats = @import("formats");
const types = @import("types.zig");
const AssetType = types.AssetType;
const AssetPath = types.AssetPath;
const asset_list = @import("asset_list.zig");
const AssetListEntry = asset_list.AssetListEntry;
const Vector2 = formats.Vector2;
const Vector3 = formats.Vector3;
const za = @import("zalgebra");
const Vec3 = za.Vec3;
const Mat4 = za.Mat4;
const c = @cImport({
    @cInclude("assimp/cimport.h");
    @cInclude("assimp/scene.h");
    @cInclude("assimp/mesh.h");
    @cInclude("assimp/material.h");
    @cInclude("assimp/postprocess.h");

    @cInclude("stb_image.h");

    @cInclude("ispc_texcomp.h");
});

const ASSET_MAX_BYTES = 1024 * 1024 * 1024;

const scene_formats = [_][]const u8{
    ".obj",
    ".fbx",
    ".gltf",
    ".glb",
};

pub fn resolveAssetTypeByExtension(path: []const u8) ?AssetType {
    for (scene_formats) |ext| {
        if (std.mem.endsWith(u8, path, ext)) {
            return .Scene;
        }
    }
    if (std.mem.endsWith(u8, path, ".prog")) {
        return .ShaderProgram;
    }
    if (std.mem.endsWith(u8, path, ".glsl")) {
        return .Shader;
    }
    if (std.mem.endsWith(u8, path, ".png") or std.mem.endsWith(u8, path, ".jpg") or std.mem.endsWith(u8, path, ".exr")) {
        return .Texture;
    }
    return null;
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const argv = std.os.argv;
    if (argv.len < 3) {
        std.log.err("usage assetc <rel_path> <input> <output>\n", .{});
        return error.MissingArgs;
    }

    const abs_input = std.mem.span(argv[argv.len - 2]);
    const output_dir_path = std.mem.span(argv[argv.len - 1]);

    // HACK: build.zig gives us a path like: zig-cache/o/<hash>/assets
    // assetc outputs paths including the "assets/" prefix, so we do an equivalent
    // of `cd ..` to avoid the "assets/assets/" prefix
    const output_dirname = std.fs.path.dirname(output_dir_path) orelse return error.EmptyOutputPath;

    var cwd_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const cwd_path = try std.os.getcwd(&cwd_buf);

    const rel_input = try std.fs.path.relative(allocator, cwd_path, abs_input);
    const rel_output = try std.fs.path.relative(allocator, cwd_path, output_dirname);

    var output_dir = try std.fs.cwd().makeOpenPath(rel_output, .{});
    defer output_dir.close();

    const asset_type = resolveAssetTypeByExtension(abs_input) orelse return error.UnknownAssetType;

    var buf_asset_list_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const asset_list_writer = buf_asset_list_writer.writer();

    std.log.debug("type: {s}, rel_input: {s}, output_dir: {s}", .{ @tagName(asset_type), rel_input, rel_output });

    switch (asset_type) {
        .Scene => try processScene(allocator, rel_input, output_dir, asset_list_writer),
        .Shader => try copyFile(asset_type, rel_input, output_dir, asset_list_writer),
        .ShaderProgram => try processShaderProgram(allocator, rel_input, output_dir, asset_list_writer),
        .Texture => try processTextureFromFile(allocator, rel_input, output_dir, asset_list_writer),
        else => unreachable,
    }
    try buf_asset_list_writer.flush();
}

fn copyFile(_type: AssetType, input: []const u8, output_dir: std.fs.Dir, asset_list_writer: anytype) !void {
    const asset_path = AssetPath{ .simple = input };

    const asset_list_entry = AssetListEntry{
        .type = _type,
        .src_path = asset_path,
    };

    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const out_path = try asset_list_entry.getOutputPath(&buf);

    try output_dir.makePath(std.fs.path.dirname(out_path) orelse ".");

    try std.fs.Dir.copyFile(std.fs.cwd(), input, output_dir, out_path, .{});
    try asset_list.writeAssetListEntryText(asset_list_writer, asset_list_entry);
}

const AssetOutput = struct {
    file: std.fs.File,
    list_entry: AssetListEntry,
};

fn createOutput(_type: AssetType, asset_path: AssetPath, output_dir: std.fs.Dir, writer: anytype) !AssetOutput {
    const asset_list_entry = AssetListEntry{
        .type = _type,
        .src_path = asset_path,
    };

    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const out_path = try asset_list_entry.getOutputPath(&buf);

    var output_subdir = try output_dir.makeOpenPath(std.fs.path.dirname(out_path) orelse ".", .{});
    defer output_subdir.close();

    try asset_list.writeAssetListEntryText(writer, asset_list_entry);

    const file = try output_subdir.createFile(std.fs.path.basename(out_path), .{});

    return AssetOutput{
        .file = file,
        .list_entry = asset_list_entry,
    };
}

const AI_MATKEY_NAME = "?mat.name";
const AI_MATKEY_SHADING_MODEL = "$mat.shadingm";
const AI_MATKEY_BASE_COLOR = "$clr.base";
const AI_MATKEY_METALLIC_FACTOR = "$mat.metallicFactor";
const AI_MATKEY_ROUGHNESS_FACTOR = "$mat.roughnessFactor";
const AI_MATKEY_GLTF_PBRMETALLICROUGHNESS_METALLICROUGHNESS_TEXTURE = c.aiTextureType_UNKNOWN;

/// This can output either a single mesh (for simple formats like obj)
/// or a scene + a bunch of sub assets (meshes, materials, textures, animations, etc.)
/// It all depends on the source asset.
fn processScene(allocator: std.mem.Allocator, input: []const u8, output_dir: std.fs.Dir, asset_list_writer: anytype) !void {
    const input_z = try std.mem.concatWithSentinel(allocator, u8, &.{input}, 0);
    const input_dir = std.fs.path.dirname(input) orelse "";
    // const config: *c.aiPropertyStore = @as(?*c.aiPropertyStore, @ptrCast(c.aiCreatePropertyStore())) orelse return error.PropertyStore;
    // defer c.aiReleasePropertyStore(config);

    // // Remove point and line meshes
    // c.aiSetImportPropertyInteger(config, c.AI_CONFIG_PP_SBP_REMOVE, c.aiPrimitiveType_POINT | c.aiPrimitiveType_LINE);

    const maybe_scene: ?*const c.aiScene = @ptrCast(c.aiImportFile(
        input_z.ptr,
        @as(c_uint, @intCast(c.aiProcess_CalcTangentSpace | c.aiProcess_Triangulate | c.aiProcess_JoinIdenticalVertices | c.aiProcess_SortByPType | c.aiProcess_GenNormals)) | c.aiProcess_GenBoundingBoxes,
    ));
    if (maybe_scene == null) {
        std.log.err("assimp import error: {s}\n", .{c.aiGetErrorString()});
        return error.ImportFailed;
    }
    const scene = maybe_scene.?;
    // defer c.aiReleaseImport(scene);

    if (scene.mNumMeshes == 0) return error.NoMeshes;

    const base_asset_path = AssetPath{ .simple = input };

    // Embedded textures
    var texture_outputs = try allocator.alloc(TextureOutput, @intCast(scene.mNumTextures));
    if (scene.mTextures != null) {
        const textures: []*c.aiTexture = @ptrCast(scene.mTextures[0..@intCast(scene.mNumTextures)]);

        for (textures, 0..) |texture, i| {
            if (texture.mHeight != 0) {
                std.log.debug("TODO: support loading raw textures from assimp\n", .{});
                return error.UnsupportedRawTexture;
            }

            var name: []u8 = @alignCast(texture.mFilename.data[0..texture.mFilename.length]);
            if (name.len == 0) {
                name = try std.fmt.allocPrint(allocator, "texture_{}", .{i + 1});
            }

            texture_outputs[i] = .{
                .texture = texture,
                .asset = try createOutput(.Texture, base_asset_path.subPath(name), output_dir, asset_list_writer),
            };
        }
    }

    // Materials
    var material_outputs = try allocator.alloc(formats.Material, @intCast(scene.mNumMaterials));
    if (scene.mMaterials != null) {
        const materials: []*c.aiMaterial = @ptrCast(scene.mMaterials[0..@intCast(scene.mNumMaterials)]);

        for (materials, 0..) |material, i| {
            var mat_output = formats.Material{};

            var base_color: c.aiColor4D = .{};
            if (c.aiGetMaterialColor(material, AI_MATKEY_BASE_COLOR, 0, 0, &base_color) == c.aiReturn_SUCCESS) {
                // TODO: rgba
                mat_output.albedo = Vec3.new(base_color.r, base_color.g, base_color.b);
            }

            if (c.aiGetMaterialTextureCount(material, c.aiTextureType_BASE_COLOR) > 0) {
                const mat_texture = try getMaterialTexture(allocator, input_dir, material, c.aiTextureType_BASE_COLOR, 0);
                const entry = mat_texture.path.resolveAssetListEntry(texture_outputs);
                mat_output.albedo_map.id = entry.getAssetId();
            }

            _ = c.aiGetMaterialFloat(material, AI_MATKEY_METALLIC_FACTOR, 0, 0, &mat_output.metallic);
            if (c.aiGetMaterialTextureCount(material, c.aiTextureType_METALNESS) > 0) {
                const mat_texture = try getMaterialTexture(allocator, input_dir, material, c.aiTextureType_METALNESS, 0);
                const entry = mat_texture.path.resolveAssetListEntry(texture_outputs);
                mat_output.metallic_map.id = entry.getAssetId();
            }

            _ = c.aiGetMaterialFloat(material, AI_MATKEY_ROUGHNESS_FACTOR, 0, 0, &mat_output.roughness);
            if (c.aiGetMaterialTextureCount(material, c.aiTextureType_DIFFUSE_ROUGHNESS) > 0) {
                const mat_texture = try getMaterialTexture(allocator, input_dir, material, c.aiTextureType_DIFFUSE_ROUGHNESS, 0);
                const entry = mat_texture.path.resolveAssetListEntry(texture_outputs);
                mat_output.roughness_map.id = entry.getAssetId();
            }

            if (c.aiGetMaterialTextureCount(material, c.aiTextureType_NORMALS) > 0) {
                const mat_texture = try getMaterialTexture(allocator, input_dir, material, c.aiTextureType_NORMALS, 0);
                const entry = mat_texture.path.resolveAssetListEntry(texture_outputs);
                switch (mat_texture.path) {
                    .embedded => |idx| {
                        texture_outputs[idx].tex_type = .Normal;
                    },
                    else => {},
                }
                mat_output.normal_map.id = entry.getAssetId();
            }
            material_outputs[i] = mat_output;
        }
    }

    for (texture_outputs) |tex_out| {
        defer tex_out.asset.file.close();

        try processTexture(allocator, tex_out.tex_type, @as([*]u8, @ptrCast(tex_out.texture.pcData))[0..@intCast(tex_out.texture.mWidth)], tex_out.asset.file);
    }

    const meshes: []*c.aiMesh = @ptrCast(scene.mMeshes[0..@intCast(scene.mNumMeshes)]);
    var mesh_outputs = try allocator.alloc(AssetListEntry, meshes.len);
    for (meshes, 0..) |mesh, i| {
        const name = mesh.mName.data[0..mesh.mName.length];

        var output = try createOutput(.Mesh, base_asset_path.subPath(try allocator.dupe(u8, name)), output_dir, asset_list_writer);
        defer output.file.close();

        if (mesh.mMaterialIndex < 0 or @as(usize, @intCast(mesh.mMaterialIndex)) > material_outputs.len) {
            return error.InvalidMaterialIndex;
        }

        mesh_outputs[i] = output.list_entry;

        try processMesh(allocator, scene, material_outputs, mesh, output.file);
    }

    if (scene.mRootNode == null) return;

    var node_to_entity_idx = std.AutoHashMap(*c.aiNode, usize).init(allocator);
    var entities = std.ArrayList(formats.Entity.Data).init(allocator);
    var parents = std.ArrayList(i64).init(allocator);

    // Breadth first traversal
    var nodeq = std.ArrayList(*c.aiNode).init(allocator);
    try nodeq.append(@ptrCast(scene.mRootNode));

    while (nodeq.popOrNull()) |node| {
        if (node.mChildren != null) {
            const children: []*c.aiNode = @ptrCast(node.mChildren[0..@intCast(node.mNumChildren)]);
            for (0..children.len) |i| {
                // Reverse order, because pop taks from end of the list
                const child = children[children.len - i - 1];
                try nodeq.append(child);
            }
        }

        try entities.append(.{});
        const idx = entities.items.len - 1;
        try node_to_entity_idx.put(node, idx);

        const maybe_parent: ?*c.aiNode = @ptrCast(node.mParent);
        if (maybe_parent) |parent| {
            const parent_idx = node_to_entity_idx.get(parent) orelse return error.MissingParentIdx; // this is a bug in our code
            try parents.append(@intCast(parent_idx));
        } else {
            try parents.append(-1);
        }

        const ent = &entities.items[idx];

        var mat = Mat4.fromSlice(@ptrCast(&node.mTransformation));
        mat = mat.transpose();
        const mat_decomp = mat.decompose();
        ent.transform.pos = mat_decomp.t;
        ent.transform.rot = mat_decomp.r;
        ent.transform.scale = mat_decomp.s;

        if (node.mMeshes != null) {
            const mesh_indices = node.mMeshes[0..node.mNumMeshes];
            if (mesh_indices.len == 1) {
                const mesh_entry = mesh_outputs[mesh_indices[0]];

                ent.flags.mesh = true;
                ent.mesh.handle = .{ .id = mesh_entry.getAssetId() };
            } else {
                for (mesh_indices) |mesh_idx| {
                    const mesh_entry = mesh_outputs[@intCast(mesh_idx)];

                    try entities.append(.{});
                    const sub_idx = entities.items.len - 1;
                    try parents.append(@intCast(idx));
                    const sub_ent = &entities.items[sub_idx];

                    sub_ent.flags.mesh = true;

                    sub_ent.mesh = .{
                        .handle = .{ .id = mesh_entry.getAssetId() },
                    };
                }
            }
        }
    }

    const out_scene = formats.Scene{
        .header = .{
            .entity_count = @intCast(entities.items.len),
        },
        .entities = entities.items,
        .parents = parents.items,
    };

    const output = try createOutput(.Scene, base_asset_path.subPath("scene"), output_dir, asset_list_writer);
    defer output.file.close();
    var buf_writer = std.io.bufferedWriter(output.file.writer());
    try formats.writeScene(buf_writer.writer(), out_scene, formats.native_endian);
    try buf_writer.flush();
}

const TextureOutput = struct {
    texture: *c.aiTexture,
    asset: AssetOutput,
    tex_type: TextureType = .Color,
};

const AssimpTextureRef = union(enum) {
    external: []const u8,
    embedded: usize,

    pub fn fromRelativePath(allocator: std.mem.Allocator, input_dir: []const u8, str: []const u8) !AssimpTextureRef {
        if (str.len == 0) return error.EmptyPath;

        if (str[0] == '*') {
            const idx = try std.fmt.parseInt(usize, str[1..], 10);

            return .{ .embedded = idx };
        }

        const resolved_path = try std.fs.path.resolve(allocator, &.{ input_dir, str });
        defer allocator.free(resolved_path);
        const cwd_relative_path = try std.fs.path.relative(allocator, try std.fs.cwd().realpathAlloc(allocator, "."), resolved_path);

        return .{ .external = cwd_relative_path };
    }

    pub fn resolveAssetListEntry(self: AssimpTextureRef, embedded: []const TextureOutput) AssetListEntry {
        switch (self) {
            .embedded => |idx| {
                return embedded[idx].asset.list_entry;
            },
            .external => |path| {
                // TODO: resolve relative to current input file
                return AssetListEntry{ .src_path = AssetPath.fromString(path), .type = .Texture };
            },
        }
    }
};

const MaterialTexture = struct {
    path: AssimpTextureRef = .{ .external = "" },
    mapping: c.aiTextureMapping = 0,
    uv_index: c_uint = 0,
    blend: f32 = 0,
    op: c.aiTextureOp = 0,
    map_mode: [3]c.aiTextureMapMode = .{ 0, 0, 0 },
    flags: c_uint = 0,
};
fn getMaterialTexture(allocator: std.mem.Allocator, input_dir: []const u8, material: *c.aiMaterial, _type: c.aiTextureType, index: c_uint) !MaterialTexture {
    var path: c.aiString = undefined;
    var result: MaterialTexture = .{};

    try tryAssimp(c.aiGetMaterialTexture(
        material,
        _type,
        index,
        &path,
        &result.mapping,
        &result.uv_index,
        &result.blend,
        &result.op,
        &result.map_mode,
        &result.flags,
    ));

    const path_str: []u8 = @alignCast(path.data[0..path.length]);
    result.path = try AssimpTextureRef.fromRelativePath(allocator, input_dir, path_str);

    return result;
}

fn processMesh(allocator: std.mem.Allocator, scene: *const c.aiScene, material_outputs: []const formats.Material, mesh: *const c.aiMesh, out_file: std.fs.File) !void {
    _ = scene; // autofix
    if (mesh.mNormals == null) return error.MissingNormals;
    if (mesh.mTangents == null) return error.MissingTangents;
    if (mesh.mTextureCoords[0] == null) return error.MissingUVs;
    if (mesh.mNumUVComponents[0] != 2) return error.WrongUVComponents;

    var vertices = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var normals = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var tangents = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var uvs = try allocator.alloc(Vector2, @intCast(mesh.mNumVertices));

    var indices = try allocator.alloc(formats.Index, @intCast(mesh.mNumFaces * 3)); // triangles

    for (0..mesh.mNumVertices) |i| {
        vertices[i] = .{
            .x = mesh.mVertices[i].x,
            .y = mesh.mVertices[i].y,
            .z = mesh.mVertices[i].z,
        };
        normals[i] = .{
            .x = mesh.mNormals[i].x,
            .y = mesh.mNormals[i].y,
            .z = mesh.mNormals[i].z,
        };
        tangents[i] = .{
            .x = mesh.mTangents[i].x,
            .y = mesh.mTangents[i].y,
            .z = mesh.mTangents[i].z,
        };
        uvs[i] = .{
            .x = mesh.mTextureCoords[0][i].x,
            .y = mesh.mTextureCoords[0][i].y,
        };
    }

    for (0..mesh.mNumFaces) |i| {
        if (mesh.mFaces[i].mNumIndices != 3) continue;
        for (0..3) |j| {
            const index = mesh.mFaces[i].mIndices[j];
            if (index > std.math.maxInt(formats.Index)) {
                std.log.err("indices out of range for index format: {}\n", .{index});
                return error.TimeToIncreaseIndexSize;
            }
            indices[i * 3 + j] = @intCast(index);
        }
    }

    const material = material_outputs[@intCast(mesh.mMaterialIndex)];

    const out_mesh = formats.Mesh{
        .aabb = .{
            .min = formats.Vector3{
                .x = mesh.mAABB.mMin.x,
                .y = mesh.mAABB.mMin.y,
                .z = mesh.mAABB.mMin.z,
            },
            .max = formats.Vector3{
                .x = mesh.mAABB.mMax.x,
                .y = mesh.mAABB.mMax.y,
                .z = mesh.mAABB.mMax.z,
            },
        },
        .vertices = vertices,
        .normals = normals,
        .tangents = tangents,
        .uvs = uvs,
        .indices = indices,
        .material = material,
    };

    var buf_writer = std.io.bufferedWriter(out_file.writer());
    try formats.writeMesh(
        buf_writer.writer(),
        out_mesh,
        formats.native_endian, // TODO: use target endiannes
    );
    try buf_writer.flush();
}

fn processShaderProgram(allocator: std.mem.Allocator, input: []const u8, output_dir: std.fs.Dir, asset_list_writer: anytype) !void {
    const input_dir = std.fs.path.dirname(input).?;

    var file_contents: []u8 = undefined;
    {
        const input_file = try std.fs.cwd().openFile(input, .{});
        defer input_file.close();
        file_contents = try input_file.readToEndAlloc(allocator, ASSET_MAX_BYTES);
    }
    defer allocator.free(file_contents);

    const ShaderProgram = struct {
        shader: []const u8,
        vertex: bool,
        fragment: bool,
    };
    const program = try std.json.parseFromSlice(ShaderProgram, allocator, file_contents, .{});
    defer program.deinit();

    const shader_path = try std.fs.path.resolve(allocator, &.{ input_dir, program.value.shader });

    const relative_path = try std.fs.path.relative(allocator, try std.fs.cwd().realpathAlloc(allocator, "."), shader_path);
    const shader_asset_id = types.AssetPath.fromString(relative_path).hash();
    if (shader_asset_id == 0) {
        std.log.debug("{s}\n", .{shader_path});
        return error.InvalidShaderPath;
    }

    const output = try createOutput(.ShaderProgram, AssetPath{ .simple = input }, output_dir, asset_list_writer);
    defer output.file.close();
    var buf_writer = std.io.bufferedWriter(output.file.writer());

    try formats.writeShaderProgram(buf_writer.writer(), shader_asset_id, program.value.vertex, program.value.fragment, formats.native_endian);
    try buf_writer.flush();
}
const MipLevel = struct {
    width: usize,
    height: usize,
    data: []u8,
    out_data: []const u8 = &.{},
};

fn processTextureFromFile(allocator: std.mem.Allocator, input: []const u8, output_dir: std.fs.Dir, asset_list_writer: anytype) !void {
    const output = try createOutput(.Texture, AssetPath{ .simple = input }, output_dir, asset_list_writer);
    defer output.file.close();

    const contents = try std.fs.cwd().readFileAlloc(allocator, input, ASSET_MAX_BYTES);
    const texture_type = guessTextureTypeFromName(input);

    try processTexture(allocator, texture_type, contents, output.file);
}

/// Using naming conventions
fn guessTextureTypeFromName(name: []const u8) TextureType {
    const stem = std.fs.path.stem(name);
    var buf: [std.fs.MAX_NAME_BYTES]u8 = undefined;
    const lower_stem = std.ascii.lowerString(&buf, stem);
    const sub_ext = std.fs.path.extension(lower_stem);
    if (std.mem.eql(u8, sub_ext, ".norm")) {
        return .Normal;
    }
    if (std.mem.endsWith(u8, lower_stem, "normal")) {
        return .Normal;
    }
    if (std.mem.endsWith(u8, lower_stem, "metallicroughness")) {
        return .MetallicRoughness;
    }

    return .Color;
}

const TextureType = enum {
    Color,
    Normal,
    MetallicRoughness,
    HDR,
};

fn processTexture(allocator: std.mem.Allocator, texture_type: TextureType, contents: []const u8, out_file: std.fs.File) !void {
    var width_int: c_int = 0;
    var height_int: c_int = 0;
    var comps: c_int = 0;

    c.stbi_set_flip_vertically_on_load(1);
    const rgba_data_c = c.stbi_load_from_memory(contents.ptr, @intCast(contents.len), &width_int, &height_int, &comps, 4);
    if (rgba_data_c == null) {
        return error.ImageLoadError;
    }
    defer c.stbi_image_free(rgba_data_c);

    var format = formats.Texture.Format.bc7;

    // TODO: bc4
    if (comps == 1) {}
    if (texture_type == .Normal or comps == 2) {
        format = .bc5;
    }

    const width: usize = @intCast(width_int);
    const height: usize = @intCast(height_int);

    const rgba_data = rgba_data_c[0 .. width * height * 4];

    var padded_width: usize = width;
    var padded_height: usize = height;
    var rgba_data_padded = rgba_data;
    if (width % 4 != 0 or height % 4 != 0 or width < 4 or height < 4) {
        padded_width = @max(width + width % 4, 4);
        padded_height = @max(height + height % 4, 4);

        rgba_data_padded = try allocator.alloc(u8, padded_width * padded_height * 4);

        var dst_surf = c.rgba_surface{
            .width = @intCast(padded_width),
            .height = @intCast(padded_height),
            .stride = @intCast(padded_width * 4),
            .ptr = rgba_data_padded.ptr,
        };
        var src_surf = c.rgba_surface{
            .width = @intCast(width),
            .height = @intCast(height),
            .stride = @intCast(width * 4),
            .ptr = rgba_data.ptr,
        };
        c.ReplicateBorders(&dst_surf, &src_surf, 0, 0, 32);
    }

    // if (comps == 4) {
    //     premultiplyAlpha(rgba_data);
    // }

    const data_channels: usize = if (format == .bc5) 2 else 4;
    const data = if (data_channels < 4) dropChannels(rgba_data_padded, data_channels) else rgba_data_padded;

    const mip_levels_to_gen = 1 + @as(
        u32,
        @intFromFloat(@log2(@as(f32, @floatFromInt(@max(width, height))))),
    );
    var actual_mip_count: usize = 1;

    var mip_pyramid = std.ArrayList(MipLevel).init(allocator);
    try mip_pyramid.append(MipLevel{
        .data = data,
        .width = padded_width,
        .height = padded_height,
    });

    for (1..mip_levels_to_gen) |mip_level| {
        const divisor = std.math.powi(usize, 2, mip_level) catch unreachable;
        const mip_width = padded_width / divisor;
        const mip_height = padded_height / divisor;

        if (mip_width % 4 != 0 or mip_height % 4 != 0) {
            break;
        }

        try mip_pyramid.append(
            MipLevel{
                .width = mip_width,
                .height = mip_height,
                .data = try allocator.alloc(u8, mip_width * mip_height * data_channels),
            },
        );
        actual_mip_count += 1;
    }
    std.log.debug("mip count {}\n", .{actual_mip_count});

    for (0..actual_mip_count) |mip_level| {
        const mip_data = &mip_pyramid.items[mip_level];
        if (mip_level > 0) {
            switch (data_channels) {
                2 => downsampleRGImage2X(&mip_pyramid.items[mip_level - 1], mip_data),
                4 => downsampleRGBAImage2X(&mip_pyramid.items[mip_level - 1], mip_data),
                else => unreachable,
            }
        }

        std.log.debug("mip size {}x{}", .{ mip_data.width, mip_data.height });
        mip_data.out_data = try compressBlocksAlloc(allocator, mip_data.data, data_channels, format, @intCast(comps), mip_data.width, mip_data.height);
    }

    const out_data = try allocator.alloc([]const u8, actual_mip_count);
    for (0..actual_mip_count) |mip_level| {
        out_data[mip_level] = mip_pyramid.items[mip_level].out_data;
    }

    const texture = formats.Texture{
        .header = .{
            .format = format,
            .width = @intCast(width),
            .height = @intCast(height),
            .padded_width = @intCast(padded_width),
            .padded_height = @intCast(padded_height),
            .mip_count = @intCast(actual_mip_count),
        },
        .data = out_data,
    };

    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeTexture(buf_writer.writer(), texture, formats.native_endian);
    try buf_writer.flush();
}

fn compressBlocksAlloc(
    allocator: std.mem.Allocator,
    pixels: []u8,
    components: usize, // 2 for normal maps, 4 for everything else
    format: formats.Texture.Format,
    original_components: usize, // how many components in original image. Does not match actual components
    width: usize,
    height: usize,
) ![]u8 {
    std.debug.assert(width % 4 == 0);
    std.debug.assert(height % 4 == 0);

    const blocks_x = width / 4;
    const blocks_y = height / 4;

    const rgba_surf = c.rgba_surface{
        .width = @intCast(width),
        .height = @intCast(height),
        .stride = @intCast(width * components),
        .ptr = pixels.ptr,
    };

    const output = try allocator.alloc(u8, blocks_x * blocks_y * 16);
    switch (format) {
        .bc7 => {
            var settings: c.bc7_enc_settings = .{};
            if (original_components == 4) {
                c.GetProfile_alpha_ultrafast(&settings);
            } else {
                c.GetProfile_ultrafast(&settings);
            }
            c.CompressBlocksBC7(&rgba_surf, output.ptr, &settings);
        },
        .bc5 => {
            std.debug.assert(components == 2);
            c.CompressBlocksBC5(&rgba_surf, output.ptr);
        },
        .bc6 => {
            return error.NotImplementedYet;
        },
    }

    return output;
}

fn dropChannels(rgba_data: []u8, channel_count: usize) []u8 {
    for (0..rgba_data.len / 4) |i| {
        for (0..channel_count) |j| {
            rgba_data[i * 2 + j] = rgba_data[i * 4 + j];
        }
    }

    return rgba_data[0 .. (rgba_data.len / 4) * channel_count];
}

const gamma = 2.2;
const srgb_to_linear: [256]u8 = blk: {
    @setEvalBranchQuota(10000);
    var result: [256]u8 = undefined;
    for (0..256) |i| {
        var f: f32 = @floatFromInt(i);
        f /= 255.0;
        result[i] = @intFromFloat(std.math.pow(f32, f, gamma) * 255.0);
    }
    break :blk result;
};

fn convertSrgb(img: []u8) void {
    @setRuntimeSafety(false);

    for (0..img.len / 4) |i| {
        const pixel = img[i * 4 .. i * 4 + 4];

        pixel[0] = srgb_to_linear[pixel[0]];
        pixel[1] = srgb_to_linear[pixel[1]];
        pixel[2] = srgb_to_linear[pixel[2]];
    }
}

fn premultiplyAlpha(img: []u8) void {
    for (0..img.len / 4) |i| {
        const pixel = img[i * 4 .. i * 4 + 4];

        const r = @as(f32, @floatFromInt(pixel[0])) / 255.0;
        const g = @as(f32, @floatFromInt(pixel[1])) / 255.0;
        const b = @as(f32, @floatFromInt(pixel[2])) / 255.0;
        const a = @as(f32, @floatFromInt(pixel[3])) / 255.0;

        pixel[0] = @intFromFloat(r * a * 255.0);
        pixel[1] = @intFromFloat(g * a * 255.0);
        pixel[2] = @intFromFloat(b * a * 255.0);
    }
}

inline fn vecPow(x: @Vector(4, f32), y: f32) @Vector(4, f32) {
    return @exp(@log(x) * @as(@Vector(4, f32), @splat(y)));
}

fn downsampleRGImage2X(src: *const MipLevel, dst: *const MipLevel) void {
    const srcStride = src.width * 2;
    const dstStride = dst.width * 2;
    for (0..dst.height) |y| {
        for (0..dst.width) |x| {
            const x0 = x * 2;
            const y0 = y * 2;
            var result = @Vector(2, f32){ 0, 0 };

            for (0..2) |y1| {
                for (0..2) |x1| {
                    const srcX = x0 + x1;
                    const srcY = y0 + y1;

                    result += loadColorVec2(src.data[srcY * srcStride + srcX * 2 ..]);
                }
            }

            result /= @splat(4);
            storeColorVec2(dst.data[y * dstStride + x * 2 ..], result);
        }
    }
}

fn downsampleRGBAImage2X(src: *const MipLevel, dst: *const MipLevel) void {
    const srcStride = src.width * 4;
    const dstStride = dst.width * 4;
    for (0..dst.height) |y| {
        for (0..dst.width) |x| {
            const x0 = x * 2;
            const y0 = y * 2;
            var result = @Vector(4, f32){ 0, 0, 0, 0 };

            for (0..2) |y1| {
                for (0..2) |x1| {
                    const srcX = x0 + x1;
                    const srcY = y0 + y1;

                    result += loadColorVec4(src.data[srcY * srcStride + srcX * 4 ..]);
                }
            }

            result /= @splat(4);
            storeColorVec4(dst.data[y * dstStride + x * 4 ..], result);
        }
    }
}

inline fn loadColorVec2(pixel: []const u8) @Vector(2, f32) {
    @setRuntimeSafety(false);
    std.debug.assert(pixel.len >= 2);

    return @Vector(2, f32){
        @as(f32, @floatFromInt(pixel[0])),
        @as(f32, @floatFromInt(pixel[1])),
    } / @as(@Vector(2, f32), @splat(255.0));
}

inline fn storeColorVec2(pixel: []u8, vec: @Vector(2, f32)) void {
    @setRuntimeSafety(false);
    std.debug.assert(pixel.len >= 2);

    const out = vec * @as(@Vector(2, f32), @splat(255.0));

    pixel[0] = @intFromFloat(out[0]);
    pixel[1] = @intFromFloat(out[1]);
}

inline fn loadColorVec4(pixel: []const u8) @Vector(4, f32) {
    @setRuntimeSafety(false);
    std.debug.assert(pixel.len >= 4);

    return @Vector(4, f32){
        @as(f32, @floatFromInt(pixel[0])),
        @as(f32, @floatFromInt(pixel[1])),
        @as(f32, @floatFromInt(pixel[2])),
        @as(f32, @floatFromInt(pixel[3])),
    } / @as(@Vector(4, f32), @splat(255.0));
}

inline fn storeColorVec4(pixel: []u8, vec: @Vector(4, f32)) void {
    @setRuntimeSafety(false);
    std.debug.assert(pixel.len >= 4);

    const out = vec * @as(@Vector(4, f32), @splat(255.0));

    pixel[0] = @intFromFloat(out[0]);
    pixel[1] = @intFromFloat(out[1]);
    pixel[2] = @intFromFloat(out[2]);
    pixel[3] = @intFromFloat(out[3]);
}

fn tryAssimp(code: c.aiReturn) !void {
    switch (code) {
        c.aiReturn_SUCCESS => {},
        c.aiReturn_FAILURE => {
            std.log.err("getMaterialTexture: {s}\n", .{c.aiGetErrorString()});
            return error.AssimpError;
        },
        c.aiReturn_OUTOFMEMORY => {
            return error.OutOfMemory;
        },
        else => unreachable,
    }
}
