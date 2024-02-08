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

pub const AssetId = assets.AssetId;
pub const Handle = assets.Handle;

pub const AssetManager = @This();

// const ShaderProgramHandle = struct { id: gl.GLuint };
// const handle: ShaderProgramHandle = assets.loadShaderProgram(.{ .vertex = "shaders/vertex.glsl", .fragment = "shaders/fragment.glsl" });
// assets.unloadShaderProgram(handle);

const AssetIdList = std.SegmentedList(AssetId, 4);

const SHADER_MAX_BYTES = 1024 * 1024 * 50;
const MESH_MAX_BYTES = 1024 * 1024 * 500;

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

pub fn resolveShader(self: *AssetManager, handle: Handle.Shader, shader_type: ShaderType) *const LoadedShader {
    if (handle.id == 0) return &NullShader;

    if (self.loaded_assets.getPtr(handle.id)) |asset| {
        return &asset.shader;
    }

    return self.loadShader(handle.id, shader_type);
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

// TODO: proper watching
pub fn watchChanges(self: *AssetManager) void {
    var iter = self.loaded_assets.iterator();
    while (iter.next()) |entry| {
        const gop = self.modified_times.getOrPut(self.allocator, entry.key_ptr.*) catch return;
        if (self.didUpdate(asset_manifest.getPath(entry.key_ptr.*), gop.value_ptr)) {
            // self.dependees.get
            // self.reloadAsset(entry.key_ptr.*);
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

    const vertex = self.resolveShader(program.vertex, .vertex);
    const fragment = self.resolveShader(program.fragment, .fragment);

    try self.addDependencies(id, &.{ program.vertex.id, program.fragment.id });

    const prog = gl.createProgram();
    errdefer gl.deleteProgram(prog);

    gl.attachShader(prog, vertex.shader);
    errdefer gl.detachShader(prog, vertex.shader);
    gl.attachShader(prog, fragment.shader);
    errdefer gl.detachShader(prog, fragment.shader);

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
    .shader = 0,
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
    .indices = IndexSlice{
        .buffer = 0,
        .offset = 0,
        .count = 0,
        .type = gl.UNSIGNED_SHORT,
    },
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

    var bufs = [_]gl.GLuint{ 0, 0, 0 };
    gl.createBuffers(bufs.len, &bufs);
    errdefer gl.deleteBuffers(bufs.len, &bufs);

    const vertices = bufs[0];
    std.debug.assert(vertices != 0);
    const normals = bufs[1];
    std.debug.assert(normals != 0);
    const indices = bufs[2];
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
        .indices = .{
            .buffer = indices,
            .offset = 0,
            .count = @intCast(mesh.indices.len),
            .type = gl.UNSIGNED_SHORT,
        },
    };

    try self.loaded_assets.put(self.allocator, id, .{ .mesh = loaded_mesh });
    return &self.loaded_assets.getPtr(id).?.mesh;
}

const LoadedAsset = union(enum) {
    shader: LoadedShader,
    shaderProgram: LoadedShaderProgram,
    mesh: LoadedMesh,
};

const LoadedShader = struct {
    shader: gl.GLuint = 0,
};

const LoadedShaderProgram = struct {
    program: gl.GLuint,
};

const LoadedMesh = struct {
    positions: BufferSlice,
    normals: BufferSlice,
    indices: IndexSlice,
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

fn loadShader(self: *AssetManager, id: AssetId, shader_type: ShaderType) *const LoadedShader {
    return self.loadShaderErr(id, shader_type) catch |err| {
        std.log.err("Error: {} when loading shader id {} {s}", .{ err, id, asset_manifest.getPath(id) });
        return &NullShader;
    };
}

fn loadShaderErr(self: *AssetManager, id: AssetId, shader_type: ShaderType) !*LoadedShader {
    const path = asset_manifest.getPath(id);

    const data = try self.loadFile(self.frame_arena, path, SHADER_MAX_BYTES);
    const shader = gl.createShader(shader_type.goGLType());
    errdefer gl.deleteShader(shader);
    std.debug.assert(shader != 0); // should only happen if incorect shader type is passed
    gl.shaderSource(shader, 1, &[_][*c]const u8{@ptrCast(data.bytes)}, &[_]gl.GLint{@intCast(data.bytes.len)});
    gl.compileShader(shader);
    var success: c_int = 0;
    gl.getShaderiv(shader, gl.COMPILE_STATUS, &success);
    if (success == 0) {
        var info_len: gl.GLint = 0;
        gl.getShaderiv(shader, gl.INFO_LOG_LENGTH, &info_len);
        if (info_len > 0) {
            const info_log = try self.frame_arena.allocSentinel(u8, @intCast(info_len - 1), 0);
            gl.getShaderInfoLog(shader, @intCast(info_log.len), null, info_log);
            std.log.err("ERROR::SHADER::COMPILATION_FAILED\n{s}\n{s}\n", .{ data.bytes, info_log });
        } else {
            std.log.err("ERROR::SHADER::COMPILIATION_FAILED\n{s}\nNo info log.\n", .{data.bytes});
        }
        return error.ShaderCompilationFailed;
    }

    try self.loaded_assets.put(self.allocator, id, .{ .shader = LoadedShader{ .shader = shader } });
    try self.modified_times.put(self.allocator, id, data.modified);

    return &self.loaded_assets.getPtr(id).?.shader;
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
