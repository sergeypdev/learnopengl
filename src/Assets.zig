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
const asset_manifest = @import("asset_manifest.zig");

pub const Assets = @This();

// const ShaderProgramHandle = struct { id: gl.GLuint };
// const handle: ShaderProgramHandle = assets.loadShaderProgram(.{ .vertex = "shaders/vertex.glsl", .fragment = "shaders/fragment.glsl" });
// assets.unloadShaderProgram(handle);

const SHADER_MAX_BYTES = 1024 * 1024 * 50;
const MESH_MAX_BYTES = 1024 * 1024 * 500;

const AssetId = u32;

pub const Handle = struct {
    pub const ShaderProgram = struct {
        id: AssetId = 0,

        pub fn resolve(self: ShaderProgram, assets: *Assets) gl.GLuint {
            if (self.id == 0) return 0;

            const asset = assets.loaded_assets.getPtr(self.id) orelse unreachable;

            switch (asset.*) {
                .shaderProgram => |*shader| {
                    return shader.program;
                },
                else => unreachable,
            }
        }
    };
    pub const Mesh = struct {
        id: AssetId = 0,

        // Returns a VAO
        pub fn resolve(self: Mesh, assets: *Assets) *const LoadedMesh {
            if (assets.loaded_assets.getPtr(self.id)) |asset| {
                switch (asset.*) {
                    .mesh => |*mesh| {
                        return mesh;
                    },
                    else => unreachable,
                }
            }

            return loadMesh(assets, self.id);
        }
    };
};

allocator: std.mem.Allocator,
frame_arena: std.mem.Allocator,

// All assets are relative to exe dir
exe_dir: std.fs.Dir,

loaded_assets: std.AutoHashMapUnmanaged(AssetId, LoadedAsset) = .{},

next_id: AssetId = 10,

pub fn init(allocator: std.mem.Allocator, frame_arena: std.mem.Allocator) Assets {
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const exe_dir_path = std.fs.selfExeDirPath(&buf) catch @panic("can't find self exe dir path");
    const exe_dir = std.fs.openDirAbsolute(exe_dir_path, .{}) catch @panic("can't open self exe dir path");

    return .{
        .allocator = allocator,
        .frame_arena = frame_arena,
        .exe_dir = exe_dir,
    };
}

pub fn deinit(self: *Assets) void {
    self.loaded_assets.deinit(self.allocator);
}

// TODO: proper watching
pub fn watchChanges(self: *Assets) void {
    var iter = self.loaded_assets.iterator();
    while (iter.next()) |entry| {
        switch (entry.value_ptr.*) {
            .shaderProgram => |*shader| {
                if (self.didUpdate(shader.definition.vertex, &shader.vert_modified) or self.didUpdate(shader.definition.fragment, &shader.frag_modified)) {
                    self.reloadAsset(entry.key_ptr.*);
                }
            },
            .mesh => |*mesh| {
                if (self.didUpdate(asset_manifest.getPath(entry.key_ptr.*), &mesh.modified)) {
                    self.reloadAsset(entry.key_ptr.*);
                }
            },
        }
    }
}

fn didUpdate(self: *Assets, path: []const u8, last_modified: *i128) bool {
    const mod = fs_utils.getFileModifiedRelative(self.exe_dir, path) catch |err| {
        std.log.err("ERROR: {}\nfailed to check file modtime {s}\n", .{ err, path });
        return false;
    };
    const updated = mod != last_modified.*;
    last_modified.* = mod;
    return updated;
}

pub fn reloadAsset(self: *Assets, asset_id: AssetId) void {
    const asset = self.loaded_assets.getPtr(asset_id) orelse @panic("trying to reload unloaded asset");

    switch (asset.*) {
        .shaderProgram => |*shader| {
            _ = self.loadShaderProgramErr(shader.program, shader.definition) catch |err| {
                std.log.err("Failed to reload shader program {}\n", .{err});
            };
        },
        .mesh => {
            std.log.debug("reloading mesh {s}\n", .{asset_manifest.getPath(asset_id)});
            _ = self.loadMeshErr(
                asset_id,
            ) catch |err| {
                std.log.err("Fauled to reload mesh {}\n", .{err});
            };
        },
    }
}

pub const ShaderProgramDefinition = struct {
    // TODO: think about how to store paths?
    vertex: []const u8,
    fragment: []const u8,
};
pub fn loadShaderProgram(self: *Assets, params: ShaderProgramDefinition) Handle.ShaderProgram {
    const prog = gl.createProgram();
    errdefer gl.deleteProgram(prog);
    const mods = self.loadShaderProgramErr(prog, params) catch |err| {
        std.log.err("Failed to load shader program {}\nDefinition: {}\n", .{ err, params });

        return .{ .id = 0 };
    };
    const id = self.nextId();
    self.loaded_assets.put(self.allocator, id, .{
        .shaderProgram = .{
            .definition = .{
                .vertex = self.allocator.dupe(u8, params.vertex) catch @panic("OOM"),
                .fragment = self.allocator.dupe(u8, params.fragment) catch @panic("OOM"),
            },
            .program = prog,
            .vert_modified = mods.vert_modified,
            .frag_modified = mods.frag_modified,
        },
    }) catch @panic("OOM"); // handle this better

    return .{ .id = id };
}

fn loadShaderProgramErr(self: *Assets, prog: gl.GLuint, params: ShaderProgramDefinition) !struct { vert_modified: i128, frag_modified: i128 } {
    const vertex_file = try self.loadFile(self.frame_arena, params.vertex, SHADER_MAX_BYTES);
    const vertex_shader = try loadShader(self.frame_arena, .vertex, vertex_file.bytes);
    defer gl.deleteShader(vertex_shader);

    const fragment_file = try self.loadFile(self.frame_arena, params.fragment, SHADER_MAX_BYTES);
    const fragment_shader = try loadShader(self.frame_arena, .fragment, fragment_file.bytes);
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

    return .{ .vert_modified = vertex_file.modified, .frag_modified = fragment_file.modified };
}

const NullMesh = LoadedMesh{
    .modified = 0,

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

pub fn loadMesh(self: *Assets, id: AssetId) *const LoadedMesh {
    return self.loadMeshErr(id) catch |err| {
        std.log.err("Error: {} loading mesh at path: {s}", .{ err, asset_manifest.getPath(id) });
        return &NullMesh;
    };
}

fn loadMeshErr(self: *Assets, id: AssetId) !*const LoadedMesh {
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
        .modified = data.modified,

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
    return @ptrCast(&self.loaded_assets.getPtr(id).?.mesh);
}

fn nextId(self: *Assets) AssetId {
    const id = self.next_id;
    self.next_id += 1;

    return id;
}

const LoadedAsset = union(enum) {
    shaderProgram: LoadedShaderProgram,
    mesh: LoadedMesh,
};

const LoadedShaderProgram = struct {
    definition: ShaderProgramDefinition,
    // TODO: rendering abstraction in the future maybe
    program: gl.GLuint,
    vert_modified: i128,
    frag_modified: i128,
};

const LoadedMesh = struct {
    modified: i128,

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

const WatchedAssetFile = struct {
    modified: i128,
    asset_id: AssetId,
};

const ShaderType = enum {
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

fn loadFile(self: *Assets, allocator: std.mem.Allocator, path: []const u8, max_size: usize) !AssetData {
    const file = try self.exe_dir.openFile(path, .{});
    defer file.close();
    const meta = try file.metadata();
    const bytes = try file.reader().readAllAlloc(allocator, max_size);

    return .{ .bytes = bytes, .modified = meta.modified() };
}

fn loadShader(arena: std.mem.Allocator, shader_type: ShaderType, source: []const u8) !gl.GLuint {
    const shader = gl.createShader(shader_type.goGLType());
    errdefer gl.deleteShader(shader);
    std.debug.assert(shader != 0); // should only happen if incorect shader type is passed
    gl.shaderSource(shader, 1, &[_][*c]const u8{@ptrCast(source)}, &[_]gl.GLint{@intCast(source.len)});
    gl.compileShader(shader);
    var success: c_int = 0;
    gl.getShaderiv(shader, gl.COMPILE_STATUS, &success);
    if (success == 0) {
        var info_len: gl.GLint = 0;
        gl.getShaderiv(shader, gl.INFO_LOG_LENGTH, &info_len);
        if (info_len > 0) {
            const info_log = try arena.allocSentinel(u8, @intCast(info_len - 1), 0);
            gl.getShaderInfoLog(shader, @intCast(info_log.len), null, info_log);
            std.log.err("ERROR::SHADER::COMPILATION_FAILED\n{s}\n", .{info_log});
        } else {
            std.log.err("ERROR::SHADER::COMPILIATION_FAILED\nNo info log.\n", .{});
        }
        return error.ShaderCompilationFailed;
    }

    return shader;
}
