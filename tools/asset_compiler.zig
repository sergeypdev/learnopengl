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
});

const ASSET_MAX_BYTES = 1024 * 1024 * 1024;

const AssetType = enum {
    Mesh,
    Shader,
    ShaderProgram,
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
        else => return error.DontProcessShaders,
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
