const std = @import("std");
const gl = @import("gl.zig");
const c = @import("sdl.zig");
const AssetManager = @import("AssetManager.zig");
const a = @import("asset_manifest");
const globals = @import("globals.zig");
pub const Material = @import("formats.zig").Material;

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;

pub const MAX_FRAMES_QUEUED = 3;
pub const MAX_POINT_LIGHTS = 8;
pub const MAX_DRAW_COMMANDS = 4096;

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
camera_matrices: []u8 = &.{},
point_lights_ubo: gl.GLuint = 0,
point_lights: []u8 = &.{},
command_buffer: [MAX_DRAW_COMMANDS]DrawCommand = undefined,
command_count: usize = 0,
ubo_align: usize = 0,
shadow_vao: gl.GLuint = 0,
shadow_texture_array: gl.GLuint = 0,
shadow_texture_handle: gl.GLuint64 = 0,
shadow_framebuffer: gl.GLuint = 0,
shadow_matrices_buffer: gl.GLuint = 0,
shadow_matrices: CameraMatrices = .{},
cube_shadow_texture_array: gl.GLuint = 0,
cube_shadow_texture_handle: gl.GLuint64 = 0,
cube_shadow_framebuffer: gl.GLuint = 0,

pub fn init(allocator: std.mem.Allocator, frame_arena: std.mem.Allocator, assetman: *AssetManager) Render {
    var render = Render{
        .allocator = allocator,
        .frame_arena = frame_arena,
        .assetman = assetman,
    };

    var buffer_align_int: gl.GLint = 0;
    gl.getIntegerv(gl.UNIFORM_BUFFER_OFFSET_ALIGNMENT, &buffer_align_int);

    if (buffer_align_int == 0) @panic("Failed to query GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT");

    render.ubo_align = @intCast(buffer_align_int);

    {
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
    }

    const PERSISTENT_BUFFER_FLAGS: gl.GLbitfield = gl.MAP_PERSISTENT_BIT | gl.MAP_WRITE_BIT | gl.MAP_COHERENT_BIT;

    // Camera matrices ubo
    {
        gl.createBuffers(1, &render.camera_ubo);
        std.debug.assert(render.camera_ubo != 0);

        const buf_size = render.uboAlignedSizeOf(CameraMatrices) * MAX_FRAMES_QUEUED;
        gl.namedBufferStorage(
            render.camera_ubo,
            @intCast(buf_size),
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const camera_matrices_c: [*]u8 = @ptrCast(gl.mapNamedBufferRange(
            render.camera_ubo,
            0,
            @intCast(buf_size),
            PERSISTENT_BUFFER_FLAGS,
        ) orelse {
            checkGLError();
            @panic("bind camera_ubo");
        });
        render.camera_matrices = camera_matrices_c[0..buf_size];
    }

    // Point lights ubo
    {
        gl.createBuffers(1, &render.point_lights_ubo);
        std.debug.assert(render.camera_ubo != 0);

        const buf_size = render.uboAlignedSizeOf(PointLightArray) * MAX_FRAMES_QUEUED;
        gl.namedBufferStorage(
            render.point_lights_ubo,
            @intCast(buf_size),
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const point_lights_c: [*]u8 = @ptrCast(gl.mapNamedBufferRange(
            render.point_lights_ubo,
            0,
            @intCast(buf_size),
            PERSISTENT_BUFFER_FLAGS,
        ) orelse {
            checkGLError();
            @panic("bind point_lights_ubo");
        });
        render.point_lights = point_lights_c[0..buf_size];
    }

    {
        // 2D Shadow texture array
        {
            gl.createTextures(gl.TEXTURE_2D_ARRAY, 1, &render.shadow_texture_array);
            checkGLError();
            std.debug.assert(render.shadow_texture_array != 0);

            gl.textureStorage3D(render.shadow_texture_array, 1, gl.DEPTH_COMPONENT16, 2048, 2048, 1);
            checkGLError();

            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_COMPARE_FUNC, gl.LESS);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_BORDER);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_BORDER);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
            gl.textureParameterfv(render.shadow_texture_array, gl.TEXTURE_BORDER_COLOR, @ptrCast(&Vec4.one().data));
        }

        // First shadow texture handle
        {
            render.shadow_texture_handle = gl.GL_ARB_bindless_texture.getTextureHandleARB(render.shadow_texture_array);
            checkGLError();
            std.debug.assert(render.shadow_texture_handle != 0);
            gl.GL_ARB_bindless_texture.makeTextureHandleResidentARB(render.shadow_texture_handle);
            checkGLError();
        }

        // Cube Shadow texture array
        {
            gl.createTextures(gl.TEXTURE_CUBE_MAP_ARRAY, 1, &render.cube_shadow_texture_array);
            checkGLError();
            std.debug.assert(render.cube_shadow_texture_array != 0);

            gl.textureStorage3D(render.cube_shadow_texture_array, 1, gl.DEPTH_COMPONENT16, 512, 512, MAX_POINT_LIGHTS * 6);
            checkGLError();

            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_COMPARE_FUNC, gl.LESS);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
            gl.textureParameteri(render.cube_shadow_texture_array, gl.TEXTURE_WRAP_R, gl.CLAMP_TO_EDGE);
        }

        // Cube Shadow array handle
        {
            render.cube_shadow_texture_handle = gl.GL_ARB_bindless_texture.getTextureHandleARB(render.cube_shadow_texture_array);
            checkGLError();
            std.debug.assert(render.cube_shadow_texture_handle != 0);
            gl.GL_ARB_bindless_texture.makeTextureHandleResidentARB(render.cube_shadow_texture_handle);
            checkGLError();
        }

        // Shadow FBO
        {
            gl.createFramebuffers(1, &render.shadow_framebuffer);
            checkGLError();
            std.debug.assert(render.shadow_framebuffer != 0);
        }

        gl.namedFramebufferTextureLayer(render.shadow_framebuffer, gl.DEPTH_ATTACHMENT, render.shadow_texture_array, 0, 0);
        const check_fbo_status = gl.checkNamedFramebufferStatus(render.shadow_framebuffer, gl.DRAW_FRAMEBUFFER);
        if (check_fbo_status != gl.FRAMEBUFFER_COMPLETE) {
            std.log.debug("Shadow Framebuffer Incomplete: {}\n", .{check_fbo_status});
        }

        gl.createBuffers(1, &render.shadow_matrices_buffer);

        gl.namedBufferStorage(
            render.shadow_matrices_buffer,
            @sizeOf(CameraMatrices),
            null,
            gl.DYNAMIC_STORAGE_BIT,
        );

        // SHADOW VAO
        var vao: gl.GLuint = 0;
        gl.createVertexArrays(1, &vao);
        std.debug.assert(vao != 0);
        render.shadow_vao = vao;

        // positions
        // gl.vertexArrayVertexBuffer(vao, 0, vertices, 0, @sizeOf(formats.Vector3));
        gl.enableVertexArrayAttrib(vao, Attrib.Position.value());
        gl.vertexArrayAttribBinding(vao, Attrib.Position.value(), 0);
        gl.vertexArrayAttribFormat(vao, Attrib.Position.value(), 3, gl.FLOAT, gl.FALSE, 0);
    }

    return render;
}

pub fn begin(self: *Render) void {
    self.command_count = 0;
    self.tripple_buffer_index = (self.tripple_buffer_index + 1) % MAX_FRAMES_QUEUED;

    gl.enable(gl.CULL_FACE);
    gl.enable(gl.DEPTH_TEST);
    if (self.gl_fences[self.tripple_buffer_index]) |fence| {
        const syncResult = gl.clientWaitSync(fence, gl.SYNC_FLUSH_COMMANDS_BIT, 9999999999);

        switch (syncResult) {
            gl.ALREADY_SIGNALED => {
                // awesome
            },
            gl.TIMEOUT_EXPIRED => {
                // oh no, driver will crash soon :(
                std.log.err("OpenGL clientWaitSync timeout expired D:\n", .{});
                checkGLError();
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
}

pub fn getPointLights(self: *Render) *PointLightArray {
    return @alignCast(@ptrCast(self.point_lights[self.tripple_buffer_index * self.uboAlignedSizeOf(PointLightArray) ..].ptr));
}

pub fn flushUBOs(self: *Render) void {
    const idx = self.tripple_buffer_index;

    // gl.flushMappedNamedBufferRange(self.point_lights_ubo, idx * @sizeOf(PointLightArray), @sizeOf(PointLightArray));
    gl.bindBufferRange(
        gl.UNIFORM_BUFFER,
        UBO.PointLights.value(),
        self.point_lights_ubo,
        idx * self.uboAlignedSizeOf(PointLightArray),
        @intCast(self.uboAlignedSizeOf(PointLightArray)),
    );
    checkGLError();
}

pub fn draw(self: *Render, cmd: DrawCommand) void {
    self.command_buffer[self.command_count] = cmd;
    self.command_count += 1;
}

pub fn finish(self: *Render) void {
    const ginit = globals.g_init;

    const lights = self.getPointLights();

    // Light shadow maps
    {
        gl.bindVertexArray(self.shadow_vao);

        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, self.shadow_framebuffer);

        for (lights.lights[0..lights.count], 0..) |*light, i| {
            gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.shadow).program);
            // Directional light
            if (std.math.approxEqAbs(f32, light.pos.w(), 0, std.math.floatEps(f32))) {
                gl.namedFramebufferTextureLayer(self.shadow_framebuffer, gl.DEPTH_ATTACHMENT, self.shadow_texture_array, 0, 0);
                const check_fbo_status = gl.checkNamedFramebufferStatus(self.shadow_framebuffer, gl.DRAW_FRAMEBUFFER);
                if (check_fbo_status != gl.FRAMEBUFFER_COMPLETE) {
                    std.log.debug("Shadow Framebuffer Incomplete: {}\n", .{check_fbo_status});
                }

                gl.viewport(0, 0, 2048, 2048);

                const camera_matrix = &self.shadow_matrices;
                camera_matrix.* = .{
                    .projection = Mat4.orthographic(-2, 2, -2, 2, -5, 5),
                    .view = Mat4.lookAt(
                        Vec3.new(light.pos.x(), light.pos.y(), light.pos.z()).scale(-1),
                        Vec3.zero(),
                        Vec3.up(),
                    ),
                };
                light.shadow_vp = camera_matrix.projection.mul(camera_matrix.view);

                gl.namedBufferSubData(self.shadow_matrices_buffer, 0, @sizeOf(CameraMatrices), std.mem.asBytes(&self.shadow_matrices));
                checkGLError();

                gl.clear(gl.DEPTH_BUFFER_BIT);
                gl.bindBufferBase(gl.UNIFORM_BUFFER, UBO.CameraMatrices.value(), self.shadow_matrices_buffer);

                self.renderShadow();
            } else {
                // Point Light
                gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.cube_shadow).program);

                const pos = Vec3.new(light.pos.x(), light.pos.y(), light.pos.z());
                light.shadow_vp = Mat4.fromTranslate(pos.negate());
                // For each cube face
                for (cube_camera_dirs, 0..) |cam_dir, face| {
                    gl.namedFramebufferTextureLayer(self.shadow_framebuffer, gl.DEPTH_ATTACHMENT, self.cube_shadow_texture_array, 0, @intCast(i * 6 + face));
                    const check_fbo_status = gl.checkNamedFramebufferStatus(self.shadow_framebuffer, gl.DRAW_FRAMEBUFFER);
                    if (check_fbo_status != gl.FRAMEBUFFER_COMPLETE) {
                        std.log.debug("Shadow Framebuffer Incomplete: {}\n", .{check_fbo_status});
                    }

                    gl.viewport(0, 0, 512, 512);

                    const near_far = Vec2.new(0.1, 10);
                    const camera_matrix = &self.shadow_matrices;
                    camera_matrix.* = .{
                        .projection = Mat4.perspective(90, 1, near_far.x(), near_far.y()),
                        .view = Mat4.lookAt(
                            pos,
                            pos.add(cam_dir.target),
                            cam_dir.up,
                        ),
                    };
                    light.near_far = near_far;
                    gl.uniform2f(Uniform.NearFarPlanes.value(), near_far.x(), near_far.y());

                    gl.namedBufferSubData(self.shadow_matrices_buffer, 0, @sizeOf(CameraMatrices), std.mem.asBytes(&self.shadow_matrices));
                    checkGLError();

                    gl.clear(gl.DEPTH_BUFFER_BIT);
                    gl.bindBufferBase(gl.UNIFORM_BUFFER, UBO.CameraMatrices.value(), self.shadow_matrices_buffer);

                    self.renderShadow();
                }
            }
        }
    }

    // Light world space to view space
    for (lights.lights[0..lights.count]) |*light| {
        light.pos = self.camera.view_mat.mulByVec4(light.pos);
    }

    var width: c_int = 0;
    var height: c_int = 0;
    c.SDL_GL_GetDrawableSize(globals.g_init.window, &width, &height);

    gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, 0);
    gl.viewport(0, 0, width, height);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    {
        const camera_matrix: *CameraMatrices = @alignCast(@ptrCast(self.camera_matrices[self.tripple_buffer_index * self.uboAlignedSizeOf(CameraMatrices) ..].ptr));

        camera_matrix.* = .{
            .projection = self.camera.projection(),
            .view = self.camera.view_mat,
        };

        //gl.flushMappedNamedBufferRange(self.camera_ubo, idx * @sizeOf(CameraMatrices), @sizeOf(CameraMatrices));
        gl.bindBufferRange(
            gl.UNIFORM_BUFFER,
            UBO.CameraMatrices.value(),
            self.camera_ubo,
            self.tripple_buffer_index * self.uboAlignedSizeOf(CameraMatrices),
            @intCast(self.uboAlignedSizeOf(CameraMatrices)),
        );
        checkGLError();
    }

    gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.mesh).program);
    gl.bindVertexArray(self.mesh_vao);

    for (self.command_buffer[0..self.command_count]) |*cmd| {
        const mesh = self.assetman.resolveMesh(cmd.mesh);

        const material: Material = if (cmd.material_override) |mat| mat else mesh.material;

        gl.uniformMatrix4fv(Uniform.ModelMatrix.value(), 1, gl.FALSE, @ptrCast(&cmd.transform.data));
        {
            gl.uniform3fv(Uniform.Color.value(), 1, @ptrCast(&material.albedo.data));

            const albedo_map = self.assetman.resolveTexture(material.albedo_map);
            gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
                Uniform.AlbedoMap.value(),
                albedo_map.handle,
            );
            gl.uniform2fv(Uniform.AlbedoMapUVScale.value(), 1, @ptrCast(&albedo_map.uv_scale.data));
        }
        {
            const normal_map = self.assetman.resolveTexture(material.normal_map);
            gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
                Uniform.NormalMap.value(),
                normal_map.handle,
            );
            gl.uniform2fv(Uniform.NormalMapUVScale.value(), 1, @ptrCast(&normal_map.uv_scale.data));
        }
        {
            gl.uniform1fv(Uniform.Metallic.value(), 1, &material.metallic);

            const metallic_map = self.assetman.resolveTexture(material.metallic_map);
            gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
                Uniform.MetallicMap.value(),
                metallic_map.handle,
            );
            gl.uniform2fv(Uniform.MetallicMapUVScale.value(), 1, @ptrCast(&metallic_map.uv_scale.data));
        }
        {
            gl.uniform1fv(Uniform.Roughness.value(), 1, &material.roughness);

            const roughness_map = self.assetman.resolveTexture(material.roughness_map);
            gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
                Uniform.RoughnessMap.value(),
                roughness_map.handle,
            );
            gl.uniform2fv(Uniform.RoughnessMapUVScale.value(), 1, @ptrCast(&roughness_map.uv_scale.data));
        }
        {
            gl.uniform1fv(Uniform.Emission.value(), 1, &material.emission);

            const emission_map = self.assetman.resolveTexture(material.emission_map);
            gl.GL_ARB_bindless_texture.uniformHandleui64ARB(
                Uniform.EmissionMap.value(),
                emission_map.handle,
            );
            gl.uniform2fv(Uniform.EmissionMapUVScale.value(), 1, @ptrCast(&emission_map.uv_scale.data));
        }
        gl.GL_ARB_bindless_texture.uniformHandleui64ARB(Uniform.ShadowMap2D.value(), self.shadow_texture_handle);
        gl.GL_ARB_bindless_texture.uniformHandleui64ARB(Uniform.ShadowMapCube.value(), self.cube_shadow_texture_handle);

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

    self.gl_fences[self.tripple_buffer_index] = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    c.SDL_GL_SwapWindow(ginit.window);
    c.SDL_Delay(1);
}

const CubeCameraDir = struct {
    face: gl.GLenum,
    target: Vec3,
    up: Vec3,
};

const cube_camera_dirs = [6]CubeCameraDir{
    .{
        .face = gl.TEXTURE_CUBE_MAP_POSITIVE_X,
        .target = Vec3.right(),
        .up = Vec3.down(),
    },
    .{
        .face = gl.TEXTURE_CUBE_MAP_NEGATIVE_X,
        .target = Vec3.left(),
        .up = Vec3.down(),
    },
    .{
        .face = gl.TEXTURE_CUBE_MAP_POSITIVE_Y,
        .target = Vec3.up(),
        .up = Vec3.forward(),
    },
    .{
        .face = gl.TEXTURE_CUBE_MAP_NEGATIVE_Y,
        .target = Vec3.down(),
        .up = Vec3.back(),
    },
    .{
        .face = gl.TEXTURE_CUBE_MAP_POSITIVE_Z,
        .target = Vec3.forward(),
        .up = Vec3.down(),
    },
    .{
        .face = gl.TEXTURE_CUBE_MAP_NEGATIVE_Z,
        .target = Vec3.back(),
        .up = Vec3.down(),
    },
};

fn renderShadow(self: *Render) void {
    for (self.command_buffer[0..self.command_count]) |*cmd| {
        const mesh = self.assetman.resolveMesh(cmd.mesh);

        gl.uniformMatrix4fv(Uniform.ModelMatrix.value(), 1, gl.FALSE, @ptrCast(&cmd.transform.data));
        mesh.positions.bind(Render.Attrib.Position.value());
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);

        gl.drawElements(
            gl.TRIANGLES,
            mesh.indices.count,
            mesh.indices.type,
            @ptrFromInt(mesh.indices.offset),
        );
    }
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
    material_override: ?Material,
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

pub const Uniform = enum(gl.GLint) {
    ModelMatrix = 1,
    Color = 2,
    AlbedoMap = 3,
    AlbedoMapUVScale = 4,
    NormalMap = 5,
    NormalMapUVScale = 6,
    Metallic = 7,
    MetallicMap = 8,
    MetallicMapUVScale = 9,
    Roughness = 10,
    RoughnessMap = 11,
    RoughnessMapUVScale = 12,
    Emission = 13,
    EmissionMap = 14,
    EmissionMapUVScale = 15,

    ShadowMap2D = 16,
    ShadowMapCube = 17,

    NearFarPlanes = 18, // vec2 stores near and far planes for perspective projection

    pub inline fn value(self: Uniform) gl.GLint {
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
    projection: Mat4 = Mat4.identity(),
    view: Mat4 = Mat4.identity(),
};
pub const PointLight = extern struct {
    pos: Vec4, // x, y, z, w - vPos
    color_radius: Vec4, // x, y, z - color, w - radius
    shadow_vp: Mat4 = Mat4.identity(),
    near_far: Vec2 = Vec2.zero(),
};

// TODO: rename
pub const PointLightArray = extern struct {
    lights: [MAX_POINT_LIGHTS]PointLight,
    count: c_uint,
};

fn uboAlignedSizeOf(self: *const Render, comptime T: type) usize {
    return std.mem.alignForward(usize, @sizeOf(T), self.ubo_align);
}
