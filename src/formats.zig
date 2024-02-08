const std = @import("std");
const builtin = @import("builtin");
const Handle = @import("assets").Handle;

pub const native_endian = builtin.cpu.arch.endian();

pub const Vector3 = extern struct {
    x: f32,
    y: f32,
    z: f32,
};
pub const Index = u16;

pub const Mesh = struct {
    vertices: []align(1) Vector3,
    normals: []align(1) Vector3,
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

        size = ind_len * @sizeOf(Index);
        const indices = std.mem.bytesAsSlice(Index, buffer[offset .. offset + size]);
        offset += size;

        return .{
            .vertices = vertices,
            .normals = normals,
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
    for (value.indices) |i| {
        try writer.writeInt(Index, i, endian);
    }
}

pub const ShaderProgram = extern struct {
    vertex: Handle.Shader,
    fragment: Handle.Shader,

    pub fn fromBuffer(buf: []u8) *align(1) ShaderProgram {
        return @ptrCast(buf);
    }
};

pub fn writeShaderProgram(writer: anytype, vertex: u32, fragment: u32, endian: std.builtin.Endian) !void {
    try writer.writeInt(u32, vertex, endian);
    try writer.writeInt(u32, fragment, endian);
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
