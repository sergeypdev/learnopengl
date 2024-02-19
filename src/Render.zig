const std = @import("std");
const gl = @import("gl.zig");
const c = @import("sdl.zig");
const AssetManager = @import("AssetManager.zig");
const a = @import("asset_manifest");
const globals = @import("globals.zig");

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;

pub const MAX_FRAMES_QUEUED = 3;
pub const MAX_POINT_LIGHTS = 8;

pub const Render = @This();

var default_camera: Camera = .{};

allocator: std.mem.Allocator,
frame_arena: std.mem.Allocator,
assetman: *AssetManager,
camera: *Camera = &default_camera,
mesh_vao: gl.GLuint = 0,
tripple_buffer_index: usize = MAX_FRAMES_QUEUED - 1,
gl_fences: [MAX_FRAMES_QUEUED]?gl.GLsync = [_]?gl.GLsync{null} ** MAX_FRAMES_QUEUED,
camera_ubo: gl.GLuint = 0,
camera_matrices: []CameraMatrices = &.{},
point_lights_ubo: gl.GLuint = 0,
point_lights: []PointLightArray = &.{},

pub fn init(allocator: std.mem.Allocator, frame_arena: std.mem.Allocator, assetman: *AssetManager) Render {
    var render = Render{
        .allocator = allocator,
        .frame_arena = frame_arena,
        .assetman = assetman,
    };

    // MESH VAO
    var vao: gl.GLuint = 0;
    gl.createVertexArrays(1, &vao);
    std.debug.assert(vao != 0);
    render.mesh_vao = vao;

    // positions
    // gl.vertexArrayVertexBuffer(vao, 0, vertices, 0, @sizeOf(formats.Vector3));
    gl.enableVertexArrayAttrib(vao, Attrib.Position.value());
    gl.vertexArrayAttribBinding(vao, Attrib.Position.value(), 0);
    gl.vertexArrayAttribFormat(vao, Attrib.Position.value(), 3, gl.FLOAT, gl.FALSE, 0);

    // normals
    gl.enableVertexArrayAttrib(vao, Attrib.Normal.value());
    gl.vertexArrayAttribBinding(vao, Attrib.Normal.value(), 1);
    gl.vertexArrayAttribFormat(vao, Attrib.Normal.value(), 3, gl.FLOAT, gl.FALSE, 0);

    // tangents
    gl.enableVertexArrayAttrib(vao, Attrib.Tangent.value());
    gl.vertexArrayAttribBinding(vao, Attrib.Tangent.value(), 3);
    gl.vertexArrayAttribFormat(vao, Attrib.Tangent.value(), 3, gl.FLOAT, gl.FALSE, 0);

    // uvs
    gl.enableVertexArrayAttrib(vao, Attrib.UV.value());
    gl.vertexArrayAttribBinding(vao, Attrib.UV.value(), 2);
    gl.vertexArrayAttribFormat(vao, Attrib.UV.value(), 2, gl.FLOAT, gl.FALSE, 0);

    const PERSISTENT_BUFFER_FLAGS: gl.GLbitfield = gl.MAP_PERSISTENT_BIT | gl.MAP_WRITE_BIT | gl.MAP_COHERENT_BIT;

    // Camera matrices ubo
    {
        gl.createBuffers(1, &render.camera_ubo);
        std.debug.assert(render.camera_ubo != 0);

        gl.namedBufferStorage(
            render.camera_ubo,
            @sizeOf(CameraMatrices) * MAX_FRAMES_QUEUED,
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const camera_matrices_c: [*c]CameraMatrices = @alignCast(@ptrCast(gl.mapNamedBufferRange(render.camera_ubo, 0, @sizeOf(CameraMatrices) * MAX_FRAMES_QUEUED, PERSISTENT_BUFFER_FLAGS) orelse {
            checkGLError();
            @panic("bind camera_ubo");
        }));
        render.camera_matrices = camera_matrices_c[0..MAX_FRAMES_QUEUED];
    }

    // Point lights ubo
    {
        gl.createBuffers(1, &render.point_lights_ubo);
        std.debug.assert(render.camera_ubo != 0);

        gl.namedBufferStorage(
            render.point_lights_ubo,
            @sizeOf(PointLightArray) * MAX_FRAMES_QUEUED,
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const point_lights_c: [*c]PointLightArray = @alignCast(@ptrCast(gl.mapNamedBufferRange(
            render.point_lights_ubo,
            0,
            @sizeOf(PointLightArray) * MAX_FRAMES_QUEUED,
            PERSISTENT_BUFFER_FLAGS,
        ) orelse {
            checkGLError();
            @panic("bind point_lights_ubo");
        }));
        render.point_lights = point_lights_c[0..MAX_FRAMES_QUEUED];
    }

    return render;
}

pub fn begin(self: *Render) void {
    self.tripple_buffer_index = (self.tripple_buffer_index + 1) % MAX_FRAMES_QUEUED;

    gl.enable(gl.CULL_FACE);
    gl.enable(gl.DEPTH_TEST);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.mesh).program);
    gl.bindVertexArray(self.mesh_vao);

    if (self.gl_fences[self.tripple_buffer_index]) |fence| {
        const syncResult = gl.clientWaitSync(fence, gl.SYNC_FLUSH_COMMANDS_BIT, 9999999);

        switch (syncResult) {
            gl.ALREADY_SIGNALED => {
                // awesome
            },
            gl.TIMEOUT_EXPIRED => {
                // oh no, driver will crash soon :(
                std.log.err("OpenGL clientWaitSync timeout expired D:\n", .{});
            },
            gl.CONDITION_SATISFIED => {
                // awesome
            },
            gl.WAIT_FAILED => {
                checkGLError();
            },
            else => unreachable,
        }
        gl.deleteSync(fence);
        self.gl_fences[self.tripple_buffer_index] = null;
    }

    self.gl_fences[self.tripple_buffer_index] = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);

    {
        const camera_matrix = &self.camera_matrices[self.tripple_buffer_index];

        camera_matrix.* = .{
            .projection = self.camera.projection(),
            .view = self.camera.view_mat,
        };

        gl.bindBufferRange(
            gl.UNIFORM_BUFFER,
            UBO.CameraMatrices.value(),
            self.camera_ubo,
            self.tripple_buffer_index * @sizeOf(CameraMatrices),
            @sizeOf(CameraMatrices),
        );
    }

    gl.bindBufferRange(
        gl.UNIFORM_BUFFER,
        UBO.PointLights.value(),
        self.point_lights_ubo,
        self.tripple_buffer_index * @sizeOf(PointLightArray),
        @sizeOf(PointLightArray),
    );
}

pub fn getPointLights(self: *Render) *PointLightArray {
    return &self.point_lights[self.tripple_buffer_index];
}

pub fn draw(self: *Render, cmd: DrawCommand) void {
    gl.uniformMatrix4fv(1, 1, gl.FALSE, @ptrCast(&cmd.transform.data));
    gl.uniform3fv(2, 1, @ptrCast(&cmd.material.albedo.data));
    gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
        3,
        self.assetman.resolveTexture(cmd.material.albedo_map).handle,
    );

    const mesh = self.assetman.resolveMesh(cmd.mesh);
    mesh.positions.bind(Render.Attrib.Position.value());
    mesh.normals.bind(Render.Attrib.Normal.value());
    mesh.tangents.bind(Render.Attrib.Tangent.value());
    mesh.uvs.bind(Render.Attrib.UV.value());
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);
    gl.drawElements(
        gl.TRIANGLES,
        mesh.indices.count,
        mesh.indices.type,
        @ptrFromInt(mesh.indices.offset),
    );
}

pub fn finish(self: *Render) void {
    _ = self; // autofix
    const ginit = globals.g_init;
    c.SDL_GL_SwapWindow(ginit.window);
    c.SDL_Delay(1);
}

pub fn checkGLError() void {
    var err = gl.getError();
    if (err == gl.NO_ERROR) return;

    while (err != gl.NO_ERROR) : (err = gl.getError()) {
        const name = switch (err) {
            gl.INVALID_ENUM => "invalid enum",
            gl.INVALID_VALUE => "invalid value",
            gl.INVALID_OPERATION => "invalid operation",
            gl.STACK_OVERFLOW => "stack overflow",
            gl.STACK_UNDERFLOW => "stack underflow",
            gl.OUT_OF_MEMORY => "out of memory",
            gl.INVALID_FRAMEBUFFER_OPERATION => "invalid framebuffer operation",
            // binding.INVALID_FRAMEBUFFER_OPERATION_EXT => Error.InvalidFramebufferOperation,
            // binding.INVALID_FRAMEBUFFER_OPERATION_OES => Error.InvalidFramebufferOperation,
            //binding.TABLE_TOO_LARGE => "Table too large",
            // binding.TABLE_TOO_LARGE_EXT => Error.TableTooLarge,
            //binding.TEXTURE_TOO_LARGE_EXT => "Texture too large",
            else => "unknown error",
        };

        std.log.scoped(.OpenGL).err("OpenGL Failure: {s}\n", .{name});
    }
}

pub const DrawCommand = struct {
    mesh: AssetManager.Handle.Mesh,
    material: Material,
    transform: Mat4,
};

pub const Attrib = enum(gl.GLuint) {
    Position = 0,
    Normal = 1,
    UV = 2,
    Tangent = 3,

    pub inline fn value(self: Attrib) gl.GLuint {
        return @intFromEnum(self);
    }
};
pub const UBO = enum(gl.GLuint) {
    CameraMatrices = 0,
    PointLights = 1,

    pub inline fn value(self: UBO) gl.GLuint {
        return @intFromEnum(self);
    }
};

// TODO: support ortho
pub const Camera = struct {
    fovy: f32 = 60,
    aspect: f32 = 1,
    near: f32 = 0.1,
    far: f32 = 100,

    view_mat: Mat4 = Mat4.identity(),

    pub fn projection(self: *const Camera) Mat4 {
        return Mat4.perspective(self.fovy, self.aspect, self.near, self.far);
    }
};

// Should be std140
const CameraMatrices = extern struct {
    projection: Mat4,
    view: Mat4,
};
pub const PointLight = extern struct {
    pos_radius: Vec4, // x, y, z - pos, w - radius
    color_intensity: Vec4, // x, y, z - color, w - intensity
};

pub const PointLightArray = extern struct {
    lights: [MAX_POINT_LIGHTS]PointLight,
    count: c_uint,
};

pub const Material = struct {
    albedo: Vec3 = Vec3.one(),
    albedo_map: AssetManager.Handle.Texture = .{},
};
