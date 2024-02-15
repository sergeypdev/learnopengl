// TODO:
// - Don't allocate asset ids dynamically
// - Store asset memory usage on CPU and GPU
// - Use LRU to evict unused assets based on available memory
//   (if we have enough memory never free, lol)
//
// NOTE: 1
//   Renderer/Game code will touch assets each time they are used
//   so LRU should work pretty well I think and I don't have to retain asset ids
//   since they'll be pre-generated constants.
//
// NOTE: 2
//   It makes hot reloading easier because it eliminates load*() calls completely
//   Because each time an asset is used it's touched by using code, hot reload in asset
//   server only needs to free assets and not actually reload them, cause they'll be reloaded
//   next time they're used
const std = @import("std");
const gl = @import("gl.zig");
const fs_utils = @import("fs/utils.zig");
const formats = @import("formats.zig");
const asset_manifest = @import("asset_manifest");
const assets = @import("assets");
const basisu = @import("mach-basisu");

pub const AssetId = assets.AssetId;
pub const Handle = assets.Handle;

pub const AssetManager = @This();

const AssetIdList = std.SegmentedList(AssetId, 4);
const PowerOfTwo = u16;

const SHADER_MAX_BYTES = 1024 * 1024 * 50;
const MESH_MAX_BYTES = 1024 * 1024 * 500;
const TEXTURE_MAX_BYTES = 1024 * 1024 * 500;

allocator: std.mem.Allocator,
frame_arena: std.mem.Allocator,

// All assets are relative to exe dir
exe_dir: std.fs.Dir,

modified_times: std.AutoHashMapUnmanaged(AssetId, i128) = .{},
// Mapping from asset to all assets it depends on
dependencies: std.AutoHashMapUnmanaged(AssetId, std.SegmentedList(AssetId, 4)) = .{},
// Mapping from asset to all assets that depend on it
dependees: std.AutoHashMapUnmanaged(AssetId, std.SegmentedList(AssetId, 4)) = .{},
loaded_assets: std.AutoHashMapUnmanaged(AssetId, LoadedAsset) = .{},

pub fn init(allocator: std.mem.Allocator, frame_arena: std.mem.Allocator) AssetManager {
    basisu.init_transcoder();

    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const exe_dir_path = std.fs.selfExeDirPath(&buf) catch @panic("can't find self exe dir path");
    const exe_dir = std.fs.openDirAbsolute(exe_dir_path, .{}) catch @panic("can't open self exe dir path");

    return .{
        .allocator = allocator,
        .frame_arena = frame_arena,
        .exe_dir = exe_dir,
    };
}

pub fn deinit(self: *AssetManager) void {
    self.loaded_assets.deinit(self.allocator);
}

pub fn resolveShader(self: *AssetManager, handle: Handle.Shader) *const LoadedShader {
    if (handle.id == 0) return &NullShader;

    if (self.loaded_assets.getPtr(handle.id)) |asset| {
        return &asset.shader;
    }

    return self.loadShader(handle.id);
}

pub fn resolveShaderProgram(self: *AssetManager, handle: Handle.ShaderProgram) *const LoadedShaderProgram {
    if (handle.id == 0) return &NullShaderProgram;

    if (self.loaded_assets.getPtr(handle.id)) |asset| {
        switch (asset.*) {
            .shaderProgram => |*shader| {
                return shader;
            },
            else => unreachable,
        }
    }

    return self.loadShaderProgram(handle);
}

pub fn resolveMesh(self: *AssetManager, handle: Handle.Mesh) *const LoadedMesh {
    if (self.loaded_assets.getPtr(handle.id)) |asset| {
        switch (asset.*) {
            .mesh => |*mesh| {
                return mesh;
            },
            else => unreachable,
        }
    }

    return self.loadMesh(handle.id);
}

pub fn resolveTexture(self: *AssetManager, handle: Handle.Texture) *const LoadedTexture {
    if (self.loaded_assets.getPtr(handle.id)) |asset| {
        switch (asset.*) {
            .texture => |*texture| {
                return texture;
            },
            else => unreachable,
        }
    }

    return self.loadTexture(handle.id);
}

// TODO: proper watching
pub fn watchChanges(self: *AssetManager) void {
    var iter = self.loaded_assets.iterator();
    while (iter.next()) |entry| {
        const gop = self.modified_times.getOrPut(self.allocator, entry.key_ptr.*) catch return;
        if (!gop.found_existing) {
            gop.value_ptr.* = 0;
        }
        if (self.didUpdate(asset_manifest.getPath(entry.key_ptr.*), gop.value_ptr)) {
            self.unloadAssetWithDependees(entry.key_ptr.*);
        }
    }
}

fn didUpdate(self: *AssetManager, path: []const u8, last_modified: *i128) bool {
    const mod = fs_utils.getFileModifiedRelative(self.exe_dir, path) catch |err| {
        std.log.err("ERROR: {}\nfailed to check file modtime {s}\n", .{ err, path });
        return false;
    };
    const updated = mod != last_modified.*;
    last_modified.* = mod;
    return updated;
}

pub const ShaderProgramDefinition = struct {
    vertex: []const u8,
    fragment: []const u8,
};

pub fn loadShaderProgram(self: *AssetManager, handle: Handle.ShaderProgram) *const LoadedShaderProgram {
    return self.loadShaderProgramErr(handle.id) catch |err| {
        std.log.err("Failed to load shader program {}\n", .{err});

        return &NullShaderProgram;
    };
}

fn loadShaderProgramErr(self: *AssetManager, id: AssetId) !*LoadedShaderProgram {
    const data = try self.loadFile(self.frame_arena, asset_manifest.getPath(id), SHADER_MAX_BYTES);
    const program = formats.ShaderProgram.fromBuffer(data.bytes);

    if (!program.flags.vertex or !program.flags.fragment) {
        std.log.err("Can't compile shader program {s} without vertex AND fragment shaders\n", .{asset_manifest.getPath(id)});
        return error.UnsupportedShader;
    }

    // TODO: !!! this will keep shader source in memory as long as shader program is in memory
    // probably don't want this!
    const shader = self.resolveShader(program.shader);

    // TODO: !!! Will evict shader program if shader source is evicted. Only want this for watch changes, not
    // normal eviction!
    try self.addDependencies(id, &.{program.shader.id});

    const prog = gl.createProgram();
    errdefer gl.deleteProgram(prog);

    const vertex_shader = try self.compileShader(shader.source, .vertex);
    defer gl.deleteShader(vertex_shader);
    const fragment_shader = try self.compileShader(shader.source, .fragment);
    defer gl.deleteShader(fragment_shader);

    gl.attachShader(prog, vertex_shader);
    defer gl.detachShader(prog, vertex_shader);
    gl.attachShader(prog, fragment_shader);
    defer gl.detachShader(prog, fragment_shader);

    gl.linkProgram(prog);

    var success: c_int = 0;
    gl.getProgramiv(prog, gl.LINK_STATUS, &success);

    if (success == 0) {
        var info_len: gl.GLint = 0;
        gl.getProgramiv(prog, gl.INFO_LOG_LENGTH, &info_len);
        if (info_len > 0) {
            const info_log = try self.frame_arena.allocSentinel(u8, @intCast(info_len - 1), 0);
            gl.getProgramInfoLog(prog, @intCast(info_log.len), null, info_log);
            std.log.err("ERROR::PROGRAM::LINK_FAILED\n{s}\n", .{info_log});
        } else {
            std.log.err("ERROR::PROGRAM::LINK_FAILED\nNo info log.\n", .{});
        }
        return error.ProgramLinkFailed;
    }

    try self.loaded_assets.put(self.allocator, id, .{
        .shaderProgram = .{ .program = prog },
    });
    try self.modified_times.put(self.allocator, id, data.modified);

    return &self.loaded_assets.getPtr(id).?.shaderProgram;
}

const NullShader = LoadedShader{
    .source = "",
};

const NullShaderProgram = LoadedShaderProgram{
    .program = 0,
};

const NullMesh = LoadedMesh{
    .positions = BufferSlice{
        .buffer = 0,
        .offset = 0,
        .stride = 0,
    },
    .normals = BufferSlice{
        .buffer = 0,
        .offset = 0,
        .stride = 0,
    },
    .uvs = BufferSlice{
        .buffer = 0,
        .offset = 0,
        .stride = 0,
    },
    .indices = IndexSlice{
        .buffer = 0,
        .offset = 0,
        .count = 0,
        .type = gl.UNSIGNED_SHORT,
    },
};

// TODO: create empty texture instead, this will crash
const NullTexture = LoadedTexture{
    .name = 0,
    .handle = 0,
};

pub fn loadMesh(self: *AssetManager, id: AssetId) *const LoadedMesh {
    return self.loadMeshErr(id) catch |err| {
        std.log.err("Error: {} loading mesh at path: {s}", .{ err, asset_manifest.getPath(id) });
        return &NullMesh;
    };
}

fn loadMeshErr(self: *AssetManager, id: AssetId) !*const LoadedMesh {
    const path = asset_manifest.getPath(id);
    const data = try self.loadFile(self.frame_arena, path, MESH_MAX_BYTES);
    const mesh = formats.Mesh.fromBuffer(data.bytes);

    var bufs = [_]gl.GLuint{ 0, 0, 0, 0 };
    gl.createBuffers(bufs.len, &bufs);
    errdefer gl.deleteBuffers(bufs.len, &bufs);

    const vertices = bufs[0];
    std.debug.assert(vertices != 0);
    const normals = bufs[1];
    std.debug.assert(normals != 0);
    const uvs = bufs[2];
    std.debug.assert(uvs != 0);
    const indices = bufs[3];
    std.debug.assert(indices != 0);

    gl.namedBufferStorage(
        vertices,
        @intCast(mesh.vertices.len * @sizeOf(formats.Vector3)),
        @ptrCast(mesh.vertices),
        0,
    );
    gl.namedBufferStorage(
        normals,
        @intCast(mesh.normals.len * @sizeOf(formats.Vector3)),
        @ptrCast(mesh.normals),
        0,
    );
    gl.namedBufferStorage(
        uvs,
        @intCast(mesh.uvs.len * @sizeOf(formats.Vector2)),
        @ptrCast(mesh.uvs),
        0,
    );
    gl.namedBufferStorage(
        indices,
        @intCast(mesh.indices.len * @sizeOf(formats.Index)),
        @ptrCast(mesh.indices),
        0,
    );

    // gl.bindVertexBuffer(_bindingindex: GLuint, _buffer: GLuint, _offset: GLintptr, _stride: GLsizei)

    const loaded_mesh = LoadedMesh{
        .positions = .{
            .buffer = vertices,
            .offset = 0,
            .stride = @sizeOf(formats.Vector3),
        },
        .normals = .{
            .buffer = normals,
            .offset = 0,
            .stride = @sizeOf(formats.Vector3),
        },
        .uvs = .{
            .buffer = uvs,
            .offset = 0,
            .stride = @sizeOf(formats.Vector2),
        },
        .indices = .{
            .buffer = indices,
            .offset = 0,
            .count = @intCast(mesh.indices.len),
            .type = gl.UNSIGNED_INT,
        },
    };

    try self.loaded_assets.put(self.allocator, id, .{ .mesh = loaded_mesh });
    try self.modified_times.put(self.allocator, id, data.modified);
    return &self.loaded_assets.getPtr(id).?.mesh;
}

fn loadTexture(self: *AssetManager, id: AssetId) *const LoadedTexture {
    return self.loadTextureErr(id) catch |err| {
        std.log.err("Error: {} loading texture at path {s}\n", .{ err, asset_manifest.getPath(id) });

        return &NullTexture;
    };
}

fn loadTextureErr(self: *AssetManager, id: AssetId) !*const LoadedTexture {
    const path = asset_manifest.getPath(id);
    const data = try self.loadFile(self.frame_arena, path, TEXTURE_MAX_BYTES);

    const transcoder = try basisu.Transcoder.init(data.bytes);
    defer transcoder.deinit();

    std.debug.assert(transcoder.getImageCount() == 1); // Not supporting multiple textures yet

    const mip_level_count = transcoder.getImageLevelCount(0);
    const mip_0_desc = transcoder.getImageLevelDescriptor(0, 0) catch unreachable;

    var name: gl.GLuint = 0;
    gl.createTextures(gl.TEXTURE_2D, 1, &name);
    if (name == 0) {
        return error.GLCreateTexture;
    }
    errdefer gl.deleteTextures(1, &name);

    // TODO: query supported formats first in the future
    const format = basisu.Transcoder.TextureFormat.bc7_rgba;

    gl.textureStorage2D(
        name,
        @intCast(mip_level_count),
        gl.COMPRESSED_SRGB_ALPHA_BPTC_UNORM,
        @intCast(mip_0_desc.original_width),
        @intCast(mip_0_desc.original_height),
    );

    for (0..mip_level_count) |mip_level| {
        const desc = transcoder.getImageLevelDescriptor(0, @intCast(mip_level)) catch unreachable;
        const out_buf = try self.frame_arena.alloc(
            u8,
            @intCast(try transcoder.calcTranscodedSize(0, @intCast(mip_level), format)),
        );

        try transcoder.transcode(out_buf, 0, @intCast(mip_level), format, .{});

        gl.compressedTextureSubImage2D(
            name,
            @intCast(mip_level),
            0,
            0,
            @intCast(desc.original_width),
            @intCast(desc.original_height),
            gl.COMPRESSED_SRGB_ALPHA_BPTC_UNORM,
            @intCast(out_buf.len),
            @ptrCast(out_buf.ptr),
        );
    }

    const handle = gl.GL_ARB_bindless_texture.getTextureHandleARB(name);
    gl.GL_ARB_bindless_texture.makeTextureHandleResidentARB(handle);
    errdefer gl.GL_ARB_bindless_texture.makeTextureHandleNonResidentARB(handle);

    try self.loaded_assets.put(
        self.allocator,
        id,
        .{
            .texture = LoadedTexture{
                .name = name,
                .handle = handle,
            },
        },
    );
    try self.modified_times.put(self.allocator, id, data.modified);

    return &self.loaded_assets.getPtr(id).?.texture;
}

const LoadedAsset = union(enum) {
    shader: LoadedShader,
    shaderProgram: LoadedShaderProgram,
    mesh: LoadedMesh,
    texture: LoadedTexture,
};

const LoadedShader = struct {
    source: []const u8,
};

const LoadedShaderProgram = struct {
    program: gl.GLuint,
};

const LoadedMesh = struct {
    positions: BufferSlice,
    normals: BufferSlice,
    uvs: BufferSlice,
    indices: IndexSlice,
};

const LoadedTexture = struct {
    name: gl.GLuint,
    handle: gl.GLuint64,
};

pub const BufferSlice = struct {
    buffer: gl.GLuint,
    offset: gl.GLintptr,
    stride: gl.GLsizei,

    pub fn bind(self: *const BufferSlice, index: gl.GLuint) void {
        gl.bindVertexBuffer(index, self.buffer, self.offset, self.stride);
    }
};

pub const IndexSlice = struct {
    buffer: gl.GLuint,
    offset: gl.GLuint,
    count: gl.GLsizei,
    type: gl.GLenum,
};

pub const ShaderType = enum {
    vertex,
    fragment,

    pub fn goGLType(self: ShaderType) gl.GLenum {
        return switch (self) {
            .vertex => gl.VERTEX_SHADER,
            .fragment => gl.FRAGMENT_SHADER,
        };
    }

    const VERTEX_DEFINES = "#version 450 core\n#define VERTEX_SHADER 1\n#define VERTEX_EXPORT out\n";
    const FRAGMENT_DEFINES = "#version 450 core\n#define FRAGMENT_SHADER 1\n#define VERTEX_EXPORT in\n";
    pub fn getDefines(self: ShaderType) []const u8 {
        return switch (self) {
            .vertex => VERTEX_DEFINES,
            .fragment => FRAGMENT_DEFINES,
        };
    }
};

const AssetData = struct {
    bytes: []u8,
    modified: i128,
};

fn loadFile(self: *AssetManager, allocator: std.mem.Allocator, path: []const u8, max_size: usize) !AssetData {
    const file = try self.exe_dir.openFile(path, .{});
    defer file.close();
    const meta = try file.metadata();
    const bytes = try file.reader().readAllAlloc(allocator, max_size);

    return .{ .bytes = bytes, .modified = meta.modified() };
}

fn loadShader(self: *AssetManager, id: AssetId) *const LoadedShader {
    return self.loadShaderErr(id) catch |err| {
        std.log.err("Error: {} when loading shader id {} {s}", .{ err, id, asset_manifest.getPath(id) });
        return &NullShader;
    };
}

fn loadShaderErr(self: *AssetManager, id: AssetId) !*LoadedShader {
    const path = asset_manifest.getPath(id);

    const data = try self.loadFile(self.allocator, path, SHADER_MAX_BYTES);

    try self.loaded_assets.put(self.allocator, id, .{ .shader = LoadedShader{ .source = data.bytes } });
    try self.modified_times.put(self.allocator, id, data.modified);

    return &self.loaded_assets.getPtr(id).?.shader;
}

fn compileShader(self: *AssetManager, source: []const u8, shader_type: ShaderType) !gl.GLuint {
    const shader = gl.createShader(shader_type.goGLType());
    errdefer gl.deleteShader(shader);
    std.debug.assert(shader != 0); // should only happen if incorect shader type is passed
    const defines = shader_type.getDefines();
    gl.shaderSource(
        shader,
        2,
        &[_][*c]const u8{ @ptrCast(shader_type.getDefines()), @ptrCast(source) },
        &[_]gl.GLint{ @intCast(defines.len), @intCast(source.len) },
    );
    gl.compileShader(shader);
    var success: c_int = 0;
    gl.getShaderiv(shader, gl.COMPILE_STATUS, &success);
    if (success == 0) {
        var info_len: gl.GLint = 0;
        gl.getShaderiv(shader, gl.INFO_LOG_LENGTH, &info_len);
        if (info_len > 0) {
            const info_log = try self.frame_arena.allocSentinel(u8, @intCast(info_len - 1), 0);
            gl.getShaderInfoLog(shader, @intCast(info_log.len), null, info_log);
            std.log.err("ERROR::SHADER::COMPILATION_FAILED\n{s}{s}\n{s}\n", .{ defines, source, info_log });
        } else {
            std.log.err("ERROR::SHADER::COMPILIATION_FAILED\n{s}{s}\nNo info log.\n", .{ defines, source });
        }
        return error.ShaderCompilationFailed;
    }

    return shader;
}

fn addDependencies(self: *AssetManager, id: AssetId, dependencies: []const AssetId) !void {
    {
        const gop = try self.dependencies.getOrPut(self.allocator, id);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        try gop.value_ptr.appendSlice(self.allocator, dependencies);
    }

    for (dependencies) |dep| {
        const gop = try self.dependees.getOrPut(self.allocator, dep);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        try gop.value_ptr.append(self.allocator, id);
    }
}

fn deleteDependees(self: *AssetManager, id: AssetId) void {
    const dependees = self.dependees.getPtr(id) orelse return;

    var iter = dependees.iterator(0);

    while (iter.next()) |dep| {
        self.unloadAssetWithDependees(dep.*);
    }
}

fn unloadAssetWithDependees(self: *AssetManager, id: AssetId) void {
    self.deleteDependees(id);

    {
        const asset = self.loaded_assets.getPtr(id) orelse return;

        switch (asset.*) {
            .mesh => |*mesh| {
                gl.deleteBuffers(4, &[_]gl.GLuint{ mesh.positions.buffer, mesh.normals.buffer, mesh.uvs.buffer, mesh.indices.buffer });
            },
            .shader => |*shader| {
                self.allocator.free(shader.source);
            },
            .shaderProgram => |*program| {
                gl.deleteProgram(program.program);
            },
            .texture => |*texture| {
                gl.GL_ARB_bindless_texture.makeTextureHandleNonResidentARB(texture.handle);
                gl.deleteTextures(1, &texture.name);
            },
        }
    }
    _ = self.loaded_assets.remove(id);

    _ = self.dependees.remove(id);
    _ = self.dependencies.remove(id);
}
