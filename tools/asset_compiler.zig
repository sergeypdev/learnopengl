const std = @import("std");
const formats = @import("formats");
const asset_manifest = @import("asset_manifest");
const Vector2 = formats.Vector2;
const Vector3 = formats.Vector3;
const c = @cImport({
    @cInclude("assimp/cimport.h");
    @cInclude("assimp/scene.h");
    @cInclude("assimp/mesh.h");
    @cInclude("assimp/postprocess.h");

    @cInclude("stb_image.h");

    @cInclude("ispc_texcomp.h");
});

const ASSET_MAX_BYTES = 1024 * 1024 * 1024;

const AssetType = enum {
    Mesh,
    Shader,
    ShaderProgram,
    Texture,
    HDRTexture,
};

pub fn resolveAssetTypeByExtension(path: []const u8) ?AssetType {
    if (std.mem.endsWith(u8, path, ".obj")) {
        return .Mesh;
    }
    if (std.mem.endsWith(u8, path, ".prog")) {
        return .ShaderProgram;
    }
    if (std.mem.endsWith(u8, path, ".glsl")) {
        return .Shader;
    }
    if (std.mem.endsWith(u8, path, ".png") or std.mem.endsWith(u8, path, ".jpg")) {
        return .Texture;
    }
    if (std.mem.endsWith(u8, path, ".exr")) {
        return .HDRTexture;
    }
    return null;
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const argv = std.os.argv;
    if (argv.len < 3) {
        std.log.err("usage assetc <basedir> <input> <output>\n", .{});
        return error.MissingArgs;
    }

    const input = argv[argv.len - 2];
    const output = std.mem.span(argv[argv.len - 1]);

    const asset_type = resolveAssetTypeByExtension(std.mem.span(input)) orelse return error.UnknownAssetType;

    switch (asset_type) {
        .Mesh => try processMesh(allocator, input, output),
        .ShaderProgram => try processShaderProgram(allocator, std.mem.span(input), output),
        .Texture => try processTexture(allocator, input, output, false),
        .HDRTexture => try processTexture(allocator, input, output, true),
        else => return error.CantProcessAssetType,
    }
}

fn processMesh(allocator: std.mem.Allocator, input: [*:0]const u8, output: []const u8) !void {
    const maybe_scene: ?*const c.aiScene = @ptrCast(c.aiImportFile(
        input,
        c.aiProcess_CalcTangentSpace | c.aiProcess_Triangulate | c.aiProcess_JoinIdenticalVertices | c.aiProcess_SortByPType | c.aiProcess_GenNormals,
    ));
    if (maybe_scene == null) {
        std.log.err("assimp import error: {s}\n", .{c.aiGetErrorString()});
        return error.ImportFailed;
    }
    const scene = maybe_scene.?;
    defer c.aiReleaseImport(scene);

    if (scene.mNumMeshes == 0) return error.NoMeshes;
    if (scene.mNumMeshes > 1) return error.TooManyMeshes;

    const mesh: *c.aiMesh = @ptrCast(scene.mMeshes[0]);

    if (mesh.mNormals == null) return error.MissingNormals;
    if (mesh.mTextureCoords[0] == null) return error.MissingUVs;
    if (mesh.mNumUVComponents[0] != 2) return error.WrongUVComponents;

    var vertices = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var normals = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
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
        uvs[i] = .{
            .x = mesh.mTextureCoords[0][i].x,
            .y = mesh.mTextureCoords[0][i].y,
        };
    }

    for (0..mesh.mNumFaces) |i| {
        std.debug.assert(mesh.mFaces[i].mNumIndices == 3);
        for (0..3) |j| {
            const index = mesh.mFaces[i].mIndices[j];
            if (index > std.math.maxInt(formats.Index)) {
                std.log.err("indices out of range for index format: {}\n", .{index});
                return error.TimeToIncreaseIndexSize;
            }
            indices[i * 3 + j] = @intCast(index);
        }
    }

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
        .uvs = uvs,
        .indices = indices,
    };

    const out_file = try std.fs.createFileAbsolute(output, .{});
    defer out_file.close();
    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeMesh(
        buf_writer.writer(),
        out_mesh,
        formats.native_endian, // TODO: use target endiannes
    );
    try buf_writer.flush();
}

fn processShaderProgram(allocator: std.mem.Allocator, absolute_input: []const u8, output: []const u8) !void {
    var cwd_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const cwd_path = try std.os.getcwd(&cwd_buf);

    const input = try std.fs.path.relative(allocator, cwd_path, absolute_input);
    defer allocator.free(input);

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

    const shader_asset_id = asset_manifest.getAssetByPath(shader_path);
    if (shader_asset_id == 0) {
        std.log.debug("{s}\n", .{shader_path});
        return error.InvalidShaderPath;
    }

    const out_file = try std.fs.createFileAbsolute(output, .{});
    defer out_file.close();
    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeShaderProgram(buf_writer.writer(), shader_asset_id, program.value.vertex, program.value.fragment, formats.native_endian);
    try buf_writer.flush();
}

fn processTexture(allocator: std.mem.Allocator, input: [*:0]const u8, output: []const u8, hdr: bool) !void {
    _ = hdr; // autofix
    var width_int: c_int = undefined;
    var height_int: c_int = undefined;
    var comps: c_int = undefined;

    c.stbi_set_flip_vertically_on_load(1);
    // const FORCED_COMPONENTS = 3; // force rgb
    const data_c = c.stbi_load(input, &width_int, &height_int, &comps, 0);
    if (data_c == null) {
        return error.ImageLoadError;
    }
    defer c.stbi_image_free(data_c);

    const width: usize = @intCast(width_int);
    const height: usize = @intCast(height_int);

    // TODO: support textures not divisible by 4
    if (width % 4 != 0 or height % 4 != 0) {
        std.log.debug("Image size: {}X{}\n", .{ width, height });
        return error.ImageSizeShouldBeDivisibleBy4;
    }

    const blocks_x: usize = width / 4;
    const blocks_y: usize = height / 4;

    const rgba_surf = c.rgba_surface{
        .ptr = data_c,
        .width = @intCast(blocks_x),
        .height = @intCast(blocks_y),
        .stride = width_int * comps,
    };
    var settings: c.bc7_enc_settings = undefined;

    if (comps == 3) {
        c.GetProfile_fast(&settings);
    } else if (comps == 4) {
        c.GetProfile_alpha_fast(&settings);
    } else {
        std.log.debug("Channel count: {}\n", .{comps});
        return error.UnsupportedChannelCount;
    }

    const out_data = try allocator.alignedAlloc(u8, 16, blocks_x * blocks_y * 16);

    c.CompressBlocksBC7(&rgba_surf, out_data.ptr, &settings);

    const texture = formats.Texture{
        .header = .{
            .format = .bc7_srgb,
            .width = @intCast(width),
            .height = @intCast(height),
            .mip_levels = 1,
        },
        .data = out_data,
    };

    const out_file = try std.fs.createFileAbsolute(output, .{});
    defer out_file.close();
    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeTexture(buf_writer.writer(), texture);
    try buf_writer.flush();
}
