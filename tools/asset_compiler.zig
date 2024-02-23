const std = @import("std");
const formats = @import("formats");
const types = @import("types.zig");
const AssetType = types.AssetType;
const AssetPath = types.AssetPath;
const asset_list = @import("asset_list.zig");
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
    if (std.mem.endsWith(u8, path, ".png") or std.mem.endsWith(u8, path, ".jpg") or std.mem.endsWith(u8, path, ".exr")) {
        return .Texture;
    }
    return null;
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const argv = std.os.argv;
    if (argv.len < 4) {
        std.log.err("usage assetc <rel_path> <input> <output>\n", .{});
        return error.MissingArgs;
    }

    const rel_input = std.mem.span(argv[argv.len - 3]);
    const abs_input = std.mem.span(argv[argv.len - 2]);
    const output_dir_path = std.mem.span(argv[argv.len - 1]);

    var cwd_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const cwd_path = try std.os.getcwd(&cwd_buf);

    const cwd_rel_input = try std.fs.path.relative(allocator, cwd_path, abs_input);
    const cwd_rel_output_dir = try std.fs.path.relative(allocator, cwd_path, output_dir_path);

    // Generated output dir
    var output_dir = blk: {
        var base_output_dir = try std.fs.cwd().makeOpenPath(cwd_rel_output_dir, .{});
        defer base_output_dir.close();

        break :blk try base_output_dir.makeOpenPath(std.fs.path.dirname(rel_input) orelse ".", .{});
    };
    defer output_dir.close();

    const asset_type = resolveAssetTypeByExtension(abs_input) orelse return error.UnknownAssetType;

    switch (asset_type) {
        .Mesh => try processMesh(allocator, abs_input, output_dir),
        .Shader => try std.fs.Dir.copyFile(std.fs.cwd(), abs_input, output_dir, std.fs.path.basename(rel_input), .{}),
        .ShaderProgram => try processShaderProgram(allocator, abs_input, output_dir),
        .Texture => try processTexture(allocator, abs_input, output_dir, false),
        .Scene => return error.NotImplemented,
    }

    const out_writer = std.io.getStdOut().writer();

    try asset_list.writeAssetListEntryText(out_writer, .{
        .type = asset_type,
        .src_path = .{ .simple = cwd_rel_input }, // TODO: remove assets prefix
        .dst_path = cwd_rel_output_dir,
    });
}

fn processMesh(allocator: std.mem.Allocator, input: []const u8, output_dir: std.fs.Dir) !void {
    const maybe_scene: ?*const c.aiScene = @ptrCast(c.aiImportFile(
        input.ptr,
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
        .tangents = tangents,
        .uvs = uvs,
        .indices = indices,
    };

    const out_name = try changeExtensionAlloc(allocator, input, AssetType.Mesh.ext());

    const out_file = try output_dir.createFile(out_name, .{});
    defer out_file.close();
    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeMesh(
        buf_writer.writer(),
        out_mesh,
        formats.native_endian, // TODO: use target endiannes
    );
    try buf_writer.flush();
}

fn processShaderProgram(allocator: std.mem.Allocator, absolute_input: []const u8, output_dir: std.fs.Dir) !void {
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

    const relative_path = try std.fs.path.relative(allocator, try std.fs.cwd().realpathAlloc(allocator, "."), shader_path);
    const shader_asset_id = types.AssetPath.fromString(relative_path).hash();
    if (shader_asset_id == 0) {
        std.log.debug("{s}\n", .{shader_path});
        return error.InvalidShaderPath;
    }

    const out_file = try output_dir.createFile(std.fs.path.basename(absolute_input), .{});
    defer out_file.close();
    var buf_writer = std.io.bufferedWriter(out_file.writer());

    try formats.writeShaderProgram(buf_writer.writer(), shader_asset_id, program.value.vertex, program.value.fragment, formats.native_endian);
    try buf_writer.flush();
}
const MipLevel = struct {
    width: usize,
    height: usize,
    data: []u8,
    out_data: []const u8 = &.{},
};

fn processTexture(allocator: std.mem.Allocator, input: []const u8, output_dir: std.fs.Dir, hdr: bool) !void {
    _ = hdr; // autofix

    // For tex.norm.png - this will be ".norm"
    const sub_ext = std.fs.path.extension(std.fs.path.stem(input));
    const format = if (std.mem.eql(u8, sub_ext, ".norm")) formats.Texture.Format.bc5 else formats.Texture.Format.bc7;

    var width_int: c_int = undefined;
    var height_int: c_int = undefined;
    var comps: c_int = undefined;

    c.stbi_set_flip_vertically_on_load(1);
    const rgba_data_c = c.stbi_load(input.ptr, &width_int, &height_int, &comps, 4);
    if (rgba_data_c == null) {
        return error.ImageLoadError;
    }
    defer c.stbi_image_free(rgba_data_c);

    const width: usize = @intCast(width_int);
    const height: usize = @intCast(height_int);

    const rgba_data = rgba_data_c[0 .. width * height * 4];

    if (comps == 4) {
        premultiplyAlpha(rgba_data);
    }

    const data_channels: usize = if (format == .bc5) 2 else 4;
    const data = if (data_channels < 4) dropChannels(rgba_data, data_channels) else rgba_data;

    // TODO: support textures not divisible by 4
    if (width % 4 != 0 or height % 4 != 0) {
        std.log.debug("Image size: {}X{}\n", .{ width, height });
        return error.ImageSizeShouldBeDivisibleBy4;
    }

    const mip_levels_to_gen = 1 + @as(
        u32,
        @intFromFloat(@log2(@as(f32, @floatFromInt(@max(width, height))))),
    );
    var actual_mip_count: usize = 1;

    var mip_pyramid = std.ArrayList(MipLevel).init(allocator);
    try mip_pyramid.append(MipLevel{
        .data = data,
        .width = width,
        .height = height,
    });

    for (1..mip_levels_to_gen) |mip_level| {
        const divisor = std.math.powi(usize, 2, mip_level) catch unreachable;
        const mip_width = width / divisor;
        const mip_height = height / divisor;

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
            .mip_count = @intCast(actual_mip_count),
        },
        .data = out_data,
    };
    const out_name = try changeExtensionAlloc(allocator, input, AssetType.Texture.ext());
    const out_file = try output_dir.createFile(out_name, .{});
    defer out_file.close();
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
            if (original_components == 3) {
                c.GetProfile_ultrafast(&settings);
            } else if (original_components == 4) {
                c.GetProfile_alpha_ultrafast(&settings);
            } else {
                std.log.debug("Channel count: {}\n", .{original_components});
                return error.UnsupportedChannelCount;
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

fn changeExtensionAlloc(allocator: std.mem.Allocator, input: []const u8, new_ext: []const u8) ![]u8 {
    const input_basename = std.fs.path.basename(input);
    const ext = std.fs.path.extension(input_basename);
    const name_without_ext = input_basename[0 .. input_basename.len - ext.len];
    return try std.mem.concat(allocator, u8, &.{ name_without_ext, ".", new_ext });
}
