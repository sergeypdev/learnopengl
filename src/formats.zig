const std = @import("std");
const builtin = @import("builtin");
const Handle = @import("assets").Handle;
const za = @import("zalgebra");
const Vec3 = za.Vec3;
pub const Entity = @import("globals.zig").Entity;

pub const native_endian = builtin.cpu.arch.endian();

pub const Vector2 = extern struct {
    x: f32,
    y: f32,
};
pub const Vector3 = extern struct {
    x: f32,
    y: f32,
    z: f32,
};
pub const AABB = extern struct {
    min: Vector3,
    max: Vector3,
};
pub const Index = u32;

pub const Mesh = struct {
    aabb: AABB,

    vertices: []align(1) Vector3,
    normals: []align(1) Vector3,
    tangents: []align(1) Vector3,
    uvs: []align(1) Vector2,
    indices: []align(1) Index,

    // This will panic if data is incorrect
    // TODO: return error
    pub fn fromBuffer(buffer: []u8) Mesh {
        var offset: usize = 0;

        const aabb: AABB = @as(*align(1) AABB, @ptrCast(buffer[offset .. offset + @sizeOf(AABB)])).*;
        offset += @sizeOf(AABB);

        const vert_len = std.mem.readInt(
            usize,
            @ptrCast(buffer[offset .. offset + @sizeOf(usize)]),
            native_endian,
        );
        offset += @sizeOf(usize);

        const ind_len = std.mem.readInt(
            usize,
            @ptrCast(buffer[offset .. offset + @sizeOf(usize)]),
            native_endian,
        );
        offset += @sizeOf(usize);

        var size = vert_len * @sizeOf(Vector3);
        const vertices = std.mem.bytesAsSlice(Vector3, buffer[offset .. offset + size]);
        offset += size;
        const normals = std.mem.bytesAsSlice(Vector3, buffer[offset .. offset + size]);
        offset += size;
        const tangents = std.mem.bytesAsSlice(Vector3, buffer[offset .. offset + size]);
        offset += size;

        size = vert_len * @sizeOf(Vector2);
        const uvs = std.mem.bytesAsSlice(Vector2, buffer[offset .. offset + size]);
        offset += size;

        size = ind_len * @sizeOf(Index);
        const indices = std.mem.bytesAsSlice(Index, buffer[offset .. offset + size]);
        offset += size;

        return .{
            .aabb = aabb,
            .vertices = vertices,
            .normals = normals,
            .tangents = tangents,
            .uvs = uvs,
            .indices = indices,
        };
    }
};

pub fn writeMesh(writer: anytype, value: Mesh, endian: std.builtin.Endian) !void {
    std.debug.assert(value.vertices.len == value.normals.len);

    // AABB
    {
        try writeVector3(writer, value.aabb.min, endian);
        try writeVector3(writer, value.aabb.max, endian);
    }

    // Sizes
    try writer.writeInt(usize, value.vertices.len, endian);
    try writer.writeInt(usize, value.indices.len, endian);

    for (value.vertices) |v| {
        try writeVector3(writer, v, endian);
    }
    for (value.normals) |n| {
        try writeVector3(writer, n, endian);
    }
    for (value.tangents) |t| {
        try writeVector3(writer, t, endian);
    }
    for (value.uvs) |uv| {
        try writeVector2(writer, uv, endian);
    }
    for (value.indices) |i| {
        try writer.writeInt(Index, i, endian);
    }
}

pub const ShaderProgram = extern struct {
    pub const Flags = packed struct {
        vertex: bool,
        fragment: bool,
        _pad: u6 = 0,
    };
    comptime {
        if (@bitSizeOf(Flags) != 8) {
            @compileError("ShaderProgram.Flags needs to be updated");
        }
    }

    shader: Handle.Shader,
    flags: Flags,

    pub fn fromBuffer(buf: []u8) *align(1) ShaderProgram {
        return @ptrCast(buf);
    }
};

test "ShaderProgram serialization" {
    const source = ShaderProgram{
        .flags = .{ .vertex = true, .fragment = true },
        .shader = .{ .id = 123 },
    };

    var buf: [@sizeOf(ShaderProgram)]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeShaderProgram(stream.writer(), source.shader.id, source.flags.vertex, source.flags.fragment, native_endian);

    const result: *align(1) ShaderProgram = @ptrCast(&buf);

    try std.testing.expectEqual(source, result.*);
}

pub fn writeShaderProgram(writer: anytype, shader: u64, vertex: bool, fragment: bool, endian: std.builtin.Endian) !void {
    try writer.writeInt(u64, shader, endian);
    try writer.writeInt(
        u8,
        @bitCast(ShaderProgram.Flags{ .vertex = vertex, .fragment = fragment }),
        endian,
    );
}

fn writeVector2(writer: anytype, value: Vector2, endian: std.builtin.Endian) !void {
    try writeFloat(writer, value.x, endian);
    try writeFloat(writer, value.y, endian);
}
fn writeVector3(writer: anytype, value: Vector3, endian: std.builtin.Endian) !void {
    try writeFloat(writer, value.x, endian);
    try writeFloat(writer, value.y, endian);
    try writeFloat(writer, value.z, endian);
}

fn writeFloat(writer: anytype, value: f32, endian: std.builtin.Endian) !void {
    const val: u32 = @bitCast(value);
    try writer.writeInt(u32, val, endian);
}

pub const Texture = struct {
    pub const Format = enum(u32) {
        bc5, // uncorrelated 2 channel, used for normal maps
        bc6, // f16 for hdr textures
        bc7, // normal rgba textures, linear colors
    };

    pub const MAGIC = [_]u8{ 'T', 'X', 'F', 'M' };

    pub const Header = extern struct {
        magic: [4]u8 = MAGIC,
        format: Format,
        width: u32,
        height: u32,
        mip_count: u32,
    };

    header: Header,
    data: []const []const u8,

    pub inline fn mipLevels(self: *const Texture) usize {
        return self.data.len;
    }

    pub fn getMipDesc(self: *const Texture, mip_level: usize) MipDesc {
        const divisor = std.math.powi(u32, 2, @intCast(mip_level)) catch unreachable;
        return MipDesc{
            .width = self.header.width / divisor,
            .height = self.header.height / divisor,
        };
    }

    pub const MipDesc = struct {
        width: u32,
        height: u32,
    };

    // TODO: avoid allocation here
    pub fn fromBuffer(allocator: std.mem.Allocator, buf: []u8) !Texture {
        const header: *align(1) Header = @ptrCast(buf.ptr);

        if (!std.mem.eql(u8, &header.magic, &MAGIC)) {
            return error.MagicMatch;
        }

        const data = try allocator.alloc([]u8, @intCast(header.mip_count));

        var mip_level: usize = 0;
        var mip_data = buf[@sizeOf(Header)..];

        while (mip_data.len > 4) {
            const mip_len = std.mem.readInt(u32, mip_data[0..4], native_endian);
            const mip_slice = mip_data[4..@intCast(4 + mip_len)];
            data[mip_level] = mip_slice;
            mip_data = mip_data[4 + mip_slice.len ..];
            mip_level += 1;
        }

        return Texture{
            .header = header.*,
            .data = data,
        };
    }

    pub fn free(self: *Texture, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }
};

// TODO: this doesn't respect endiannes at all
pub fn writeTexture(writer: anytype, value: Texture, endian: std.builtin.Endian) !void {
    try writer.writeStruct(value.header);

    for (value.data) |mip_img| {
        try writer.writeInt(u32, @intCast(mip_img.len), endian);
        try writer.writeAll(mip_img);
    }
}

test "texture write/parse" {
    var data = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    const source = Texture{
        .header = .{
            .format = .bc7,
            .width = 123,
            .height = 234,
            .mip_count = 1,
        },
        .data = &.{&data},
    };

    var buf: [@sizeOf(Texture.Header) + data.len + 4]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeTexture(stream.writer(), source, native_endian);

    var decoded = try Texture.fromBuffer(std.testing.allocator, &buf);
    defer decoded.free(std.testing.allocator);
    try std.testing.expectEqualDeep(source, decoded);
}

pub const Scene = struct {
    pub const Header = extern struct {
        magic: [4]u8 = [_]u8{ 'S', 'C', 'N', 'F' }, // Scene Format
        entity_count: u32 = 0,
    };

    header: Header = .{},

    entities: []align(1) const Entity.Data = &.{},
    /// Store parenting info for each entity
    /// NOTE: Parent index should never be larger than entity index
    /// because entities will be created in order
    /// -1 means no parent
    parents: []align(1) const i64 = &.{},

    pub fn fromBuffer(buf: []u8) !Scene {
        const header_ptr: *align(1) Header = @ptrCast(buf.ptr);
        const header = header_ptr.*;

        if (!std.mem.eql(u8, &header.magic, "SCNF")) {
            return error.MagicMatch;
        }

        var offset: usize = @sizeOf(Header);
        var size: usize = @sizeOf(Entity.Data) * header.entity_count;
        const entities: []align(1) Entity.Data = std.mem.bytesAsSlice(Entity.Data, buf[offset .. offset + size]);
        offset += size;
        size = @sizeOf(i64) * header.entity_count;
        const parents: []align(1) i64 = std.mem.bytesAsSlice(i64, buf[offset .. offset + size]);
        offset += size;

        return Scene{
            .header = header,
            .entities = entities,
            .parents = parents,
        };
    }
};

// TODO: this doesn't respect endiannes at all
pub fn writeScene(writer: anytype, value: Scene, endian: std.builtin.Endian) !void {
    try writer.writeStruct(value.header);

    // TODO: make writeSlice?
    for (value.entities) |ent| {
        try writer.writeStruct(ent);
    }
    for (value.parents) |parentIdx| {
        try writer.writeInt(i64, parentIdx, endian);
    }
}

test "write and read scene" {
    var entities = [_]Entity.Data{
        .{
            .flags = .{ .point_light = true },
            .transform = .{},
        },
        .{
            .flags = .{ .point_light = true },
            .transform = .{ .pos = Vec3.new(1, 2, 3) },
        },
    };
    var parents = [_]i64{-1} ** entities.len;
    const source = Scene{
        .header = .{
            .entity_count = entities.len,
        },
        .entities = &entities,
        .parents = &parents,
    };

    var buf: [@sizeOf(Scene.Header) + entities.len * @sizeOf(Entity.Data) + entities.len * @sizeOf(i64)]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeScene(stream.writer(), source, native_endian);

    const decoded = try Scene.fromBuffer(&buf);
    try std.testing.expectEqualDeep(source, decoded);
}

pub const Material = extern struct {
    albedo: Vec3 = Vec3.one(),
    albedo_map: Handle.Texture = .{},
    normal_map: Handle.Texture = .{},
    metallic: f32 = 0,
    metallic_map: Handle.Texture = .{},
    roughness: f32 = 1,
    roughness_map: Handle.Texture = .{},
    emission: f32 = 0,
    emission_map: Handle.Texture = .{},
};
