const std = @import("std");
const formats = @import("formats");
const Vector3 = formats.Vector3;
const c = @cImport({
    @cInclude("assimp/cimport.h");
    @cInclude("assimp/scene.h");
    @cInclude("assimp/mesh.h");
    @cInclude("assimp/postprocess.h");
});

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const argv = std.os.argv;
    if (argv.len < 3) {
        return error.MissingArgs;
    }

    const input = std.os.argv[argv.len - 2];
    const output = std.mem.span(std.os.argv[argv.len - 1]);

    std.log.debug("input: {s}\n", .{input});

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

    var vertices = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var normals = try allocator.alloc(Vector3, @intCast(mesh.mNumVertices));
    var indices = try allocator.alloc(u16, @intCast(mesh.mNumFaces * 3)); // triangles

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
    }

    for (0..mesh.mNumFaces) |i| {
        std.debug.assert(mesh.mFaces[i].mNumIndices == 3);
        for (0..3) |j| {
            indices[i * 3 + j] = @intCast(mesh.mFaces[i].mIndices[j]);
        }
    }

    const out_mesh = formats.Mesh{
        .vertices = vertices,
        .normals = normals,
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
