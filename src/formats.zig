const std = @import("std");
const builtin = @import("builtin");
const Handle = @import("assets").Handle;

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
pub const Index = u32;

pub const Mesh = struct {
    vertices: []align(1) Vector3,
    normals: []align(1) Vector3,
    uvs: []align(1) Vector2,
    indices: []align(1) Index,

    // This will panic if data is incorrect
    // TODO: return error
    pub fn fromBuffer(buffer: []u8) Mesh {
        var offset: usize = 0;
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

        size = vert_len * @sizeOf(Vector2);
        const uvs = std.mem.bytesAsSlice(Vector2, buffer[offset .. offset + size]);
        offset += size;

        size = ind_len * @sizeOf(Index);
        const indices = std.mem.bytesAsSlice(Index, buffer[offset .. offset + size]);
        offset += size;

        return .{
            .vertices = vertices,
            .normals = normals,
            .uvs = uvs,
            .indices = indices,
        };
    }
};

pub fn writeMesh(writer: anytype, value: Mesh, endian: std.builtin.Endian) !void {
    std.debug.assert(value.vertices.len == value.normals.len);

    try writer.writeInt(usize, value.vertices.len, endian);
    try writer.writeInt(usize, value.indices.len, endian);

    for (value.vertices) |v| {
        try writeVector3(writer, v, endian);
    }
    for (value.normals) |n| {
        try writeVector3(writer, n, endian);
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

pub fn writeShaderProgram(writer: anytype, shader: u32, vertex: bool, fragment: bool, endian: std.builtin.Endian) !void {
    try writer.writeInt(u32, shader, endian);
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
