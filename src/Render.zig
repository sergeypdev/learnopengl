const std = @import("std");
const gl = @import("gl.zig");
const c = @import("sdl.zig");
const AssetManager = @import("AssetManager.zig");
const a = @import("asset_manifest");
const globals = @import("globals.zig");
pub const Material = @import("formats.zig").Material;
const math = @import("math.zig");

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;
const Vec2_i32 = za.Vec2_i32;

pub const MAX_FRAMES_QUEUED = 3;
pub const MAX_LIGHTS = 8;
pub const MAX_DRAW_COMMANDS = 4096;
pub const MAX_LIGHT_COMMANDS = 2048;
pub const MAX_MATERIALS = MAX_DRAW_COMMANDS;
pub const CSM_SPLITS = 4;
pub const DIRECTIONAL_SHADOW_MAP_SIZE = 2048;
// affects how cascades are split
// 0 - uniform
// 1 - exponential
// 0.5 - mix between the two
pub const CSM_EXPO_UNIFORM_FACTOR = 0.5;

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
lights: [MAX_LIGHT_COMMANDS]LightCommand = undefined,
light_count: usize = 0,

lights_ssbo: LightSSBO = .{},
materials_pbr_ssbo: MaterialPBRSSBO = .{},
draw_cmd_data_ssbo: DrawCommandDataSSBO = .{},

command_buffer: [MAX_DRAW_COMMANDS]DrawCommand = undefined,
command_count: usize = 0,

ubo_align: usize = 0,
ssbo_align: usize = 0,
shadow_vao: gl.GLuint = 0,
shadow_texture_array: gl.GLuint = 0,
shadow_texture_handle: gl.GLuint64 = 0,
shadow_framebuffer: gl.GLuint = 0,
shadow_matrices_buffer: gl.GLuint = 0,
shadow_matrices: CameraMatrices = .{},
cube_shadow_texture_array: gl.GLuint = 0,
cube_shadow_texture_handle: gl.GLuint64 = 0,
cube_shadow_framebuffer: gl.GLuint = 0,

// Destination for all 3d rendering
screen_color_texture: gl.GLuint = 0,
screen_depth_texture: gl.GLuint = 0,
screen_fbo: gl.GLuint = 0,
screen_tex_size: Vec2_i32 = Vec2_i32.zero(),
screen_mip_count: usize = 1,

// VAO for post processing shaders
post_process_vao: gl.GLuint = 0,

draw_indirect_buffer: gl.GLuint = 0,

// Bloom
screen_bloom_sampler: gl.GLuint = 0,

update_view_frustum: bool = true,
camera_view_proj: Mat4 = Mat4.identity(),
world_camera_frustum: math.Frustum = .{},
world_view_frustum_corners: [CSM_SPLITS][8]Vec3 = undefined,

pub fn init(allocator: std.mem.Allocator, frame_arena: std.mem.Allocator, assetman: *AssetManager) Render {
    var render = Render{
        .allocator = allocator,
        .frame_arena = frame_arena,
        .assetman = assetman,
    };

    gl.clipControl(gl.LOWER_LEFT, gl.ZERO_TO_ONE); // use [0, 1] depth in NDC

    {
        var buffer_align_int: gl.GLint = 0;
        gl.getIntegerv(gl.UNIFORM_BUFFER_OFFSET_ALIGNMENT, &buffer_align_int);
        if (buffer_align_int == 0) @panic("Failed to query GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT");
        render.ubo_align = @intCast(buffer_align_int);
    }

    {
        var buffer_align_int: gl.GLint = 0;
        gl.getIntegerv(gl.SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT, &buffer_align_int);
        if (buffer_align_int == 0) @panic("Failed to query GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT");
        render.ssbo_align = @intCast(buffer_align_int);
    }

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

    // SSBOs
    {
        render.lights_ssbo = LightSSBO.init(render.ssbo_align, MAX_LIGHTS, MAX_FRAMES_QUEUED) catch @panic("LightSSBO.init()");
        render.materials_pbr_ssbo = MaterialPBRSSBO.init(render.ssbo_align, MAX_MATERIALS, MAX_FRAMES_QUEUED) catch @panic("MaterialPBRSSBO.init()");
        render.draw_cmd_data_ssbo = DrawCommandDataSSBO.init(render.ssbo_align, MAX_DRAW_COMMANDS, MAX_FRAMES_QUEUED) catch @panic("DrawCommandDataSSBO.init()");
    }

    {
        // 2D Shadow texture array
        {
            gl.createTextures(gl.TEXTURE_2D_ARRAY, 1, &render.shadow_texture_array);
            checkGLError();
            std.debug.assert(render.shadow_texture_array != 0);

            gl.textureStorage3D(render.shadow_texture_array, 1, gl.DEPTH_COMPONENT16, DIRECTIONAL_SHADOW_MAP_SIZE, DIRECTIONAL_SHADOW_MAP_SIZE, CSM_SPLITS);
            checkGLError();

            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_COMPARE_FUNC, gl.LESS);

            var border = [_]f32{1} ** 4;
            gl.textureParameterfv(render.shadow_texture_array, gl.TEXTURE_BORDER_COLOR, &border);
            checkGLError();

            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_BORDER);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_BORDER);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.textureParameteri(render.shadow_texture_array, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
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

            gl.textureStorage3D(render.cube_shadow_texture_array, 1, gl.DEPTH_COMPONENT16, 512, 512, MAX_LIGHTS * 6);
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
            gl.namedFramebufferDrawBuffer(render.shadow_framebuffer, gl.NONE);
            gl.namedFramebufferReadBuffer(render.shadow_framebuffer, gl.NONE);
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

    // Screen HDR FBO
    {
        gl.createFramebuffers(1, &render.screen_fbo);
        std.debug.assert(render.screen_fbo != 0);

        var width: c_int = 0;
        var height: c_int = 0;
        c.SDL_GL_GetDrawableSize(globals.g_init.window, &width, &height);

        var textures = [2]gl.GLuint{ 0, 0 };
        gl.createTextures(gl.TEXTURE_2D, textures.len, &textures);
        render.screen_color_texture = textures[0];
        render.screen_depth_texture = textures[1];

        std.debug.assert(render.screen_color_texture != 0);
        std.debug.assert(render.screen_depth_texture != 0);

        gl.textureParameteri(render.screen_color_texture, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.textureParameteri(render.screen_color_texture, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.textureParameteri(render.screen_color_texture, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.textureParameteri(render.screen_color_texture, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

        gl.textureParameteri(render.screen_depth_texture, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.textureParameteri(render.screen_depth_texture, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.textureParameteri(render.screen_depth_texture, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.textureParameteri(render.screen_depth_texture, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

        render.updateScreenBufferSize(width, height);
    }

    // Bloom screen sampler
    {
        var sampler: gl.GLuint = 0;
        gl.createSamplers(1, &sampler);
        std.debug.assert(sampler != 0);
        render.screen_bloom_sampler = sampler;

        gl.samplerParameteri(sampler, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);
        gl.samplerParameteri(sampler, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
        gl.samplerParameteri(sampler, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.samplerParameteri(sampler, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    }

    // Post process VAO
    {
        gl.createVertexArrays(1, &render.post_process_vao);
        std.debug.assert(render.post_process_vao != 0);
        const vao = render.post_process_vao;

        // positions
        gl.enableVertexArrayAttrib(vao, Attrib.Position.value());
        gl.vertexArrayAttribBinding(vao, Attrib.Position.value(), 0);
        gl.vertexArrayAttribFormat(vao, Attrib.Position.value(), 3, gl.FLOAT, gl.FALSE, 0);
    }

    // Draw indirect buffer
    {
        gl.createBuffers(1, &render.draw_indirect_buffer);
        std.debug.assert(render.draw_indirect_buffer != 0);

        gl.namedBufferStorage(render.draw_indirect_buffer, @sizeOf(DrawIndirectCmd) * MAX_DRAW_COMMANDS, null, gl.MAP_WRITE_BIT);
    }

    return render;
}

fn getMipSize(width: i32, height: i32, mip_level: usize) Vec2_i32 {
    if (mip_level == 0) return Vec2_i32.new(width, height);
    const denom = std.math.pow(f32, 2, @floatFromInt(mip_level));
    var mip_width: c_int = @intFromFloat(@as(f32, @floatFromInt(width)) / denom);
    var mip_height: c_int = @intFromFloat(@as(f32, @floatFromInt(height)) / denom);
    mip_width = @max(mip_width, 1);
    mip_height = @max(mip_height, 1);

    return Vec2_i32.new(mip_width, mip_height);
}

fn updateScreenBufferSize(self: *Render, width: c_int, height: c_int) void {
    const mip_count = 1 + @as(
        u32,
        @intFromFloat(@log2(@as(f32, @floatFromInt(@max(width, height))))),
    );

    gl.bindTexture(gl.TEXTURE_2D, self.screen_color_texture);
    for (0..mip_count) |mip_level| {
        const size = getMipSize(width, height, mip_level);
        std.log.debug("screen_color mip {} size {}x{}\n", .{ mip_level, size.x(), size.y() });

        gl.texImage2D(gl.TEXTURE_2D, @intCast(mip_level), gl.RGB16F, size.x(), size.y(), 0, gl.RGB, gl.HALF_FLOAT, null);
        checkGLError();
    }

    // Depth doesn't need any mips cause it's not filterable anyway
    gl.bindTexture(gl.TEXTURE_2D, self.screen_depth_texture);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.DEPTH_COMPONENT32F, width, height, 0, gl.DEPTH_COMPONENT, gl.FLOAT, null);
    checkGLError();

    self.screen_tex_size = Vec2_i32.new(width, height);
    self.screen_mip_count = mip_count;
}

pub fn begin(self: *Render) void {
    self.command_count = 0;
    self.light_count = 0;
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

// TODO: get rid of this
pub fn flushUBOs(self: *Render) void {
    const idx = self.tripple_buffer_index;

    self.lights_ssbo.bind(idx, SSBO.PointLights);
    self.materials_pbr_ssbo.bind(idx, SSBO.Materials);
    self.draw_cmd_data_ssbo.bind(idx, SSBO.DrawCommandData);
}

pub const LightKind = enum {
    directional,
    point,
    // Spot, // TODO
};

pub const PointLight = struct {
    color: Vec3,
    pos: Vec3,
    radius: f32,
};

pub const LightCommand = union(LightKind) {
    directional: struct {
        color: Vec3,
        dir: Vec3,
    },
    point: PointLight,
};

pub fn drawLight(self: *Render, cmd: LightCommand) void {
    self.lights[self.light_count] = cmd;
    self.light_count += 1;
}

pub fn draw(self: *Render, cmd: DrawCommand) void {
    self.command_buffer[self.command_count] = cmd;
    self.command_count += 1;
}

pub fn finish(self: *Render) void {
    const ginit = globals.g_init;

    const camera_projection = self.camera.projection();
    const view_proj = camera_projection.mul(self.camera.view_mat);

    // Sort draw calls: opaque -> blended
    {
        const cmds = self.command_buffer[0..self.command_count];
        std.mem.sortUnstable(DrawCommand, cmds, self, struct {
            pub fn lessThan(render: *const Render, lhs: DrawCommand, rhs: DrawCommand) bool {
                const lhs_mesh = render.assetman.resolveMesh(lhs.mesh);
                const rhs_mesh = render.assetman.resolveMesh(rhs.mesh);
                const lhs_material: Material = if (lhs.material_override) |mat| mat else lhs_mesh.material;
                const rhs_material: Material = if (rhs.material_override) |mat| mat else rhs_mesh.material;
                return switch (lhs_material.blend_mode) {
                    .Opaque => switch (rhs_material.blend_mode) {
                        .AlphaBlend => true,
                        .Opaque => false,
                    },
                    .AlphaBlend => switch (rhs_material.blend_mode) {
                        .Opaque => false,
                        .AlphaBlend => {
                            const lhs_view_pos = render.camera.view_mat.mulByVec4(lhs.transform.extractTranslation().toVec4(1));
                            const rhs_view_pos = render.camera.view_mat.mulByVec4(rhs.transform.extractTranslation().toVec4(1));

                            // Back to front sorting. View pos has negative Z
                            return lhs_view_pos.z() < rhs_view_pos.z();
                        },
                    },
                };
            }
        }.lessThan);
    }

    if (self.update_view_frustum) {
        self.camera_view_proj = view_proj;
        self.world_camera_frustum = math.Frustum.new(view_proj);
    }

    const lights = self.lights[0..self.light_count];

    // Sort lights: directional first
    {
        std.mem.sortUnstable(LightCommand, lights, {}, struct {
            pub fn lessThan(_: void, lhs: LightCommand, rhs: LightCommand) bool {
                _ = rhs; // autofix
                return switch (lhs) {
                    .directional => true,
                    .point => false,
                };
            }
        }.lessThan);
    }

    const lights_buf = self.lights_ssbo.getInstance(self.tripple_buffer_index);
    lights_buf.count.* = 0;

    var dir_view_proj_mat: [CSM_SPLITS]Mat4 = undefined;

    // Light shadow maps
    {
        gl.enable(gl.DEPTH_CLAMP);
        defer gl.disable(gl.DEPTH_CLAMP);

        gl.bindVertexArray(self.shadow_vao);
        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, self.shadow_framebuffer);

        var finished_dir_lights = false;
        gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.shadow).program);

        for (lights) |light_cmd| {
            const i = lights_buf.count.*;
            if (i == lights_buf.data.len) break;

            const light = &lights_buf.data[i];
            lights_buf.count.* += 1;

            switch (light_cmd) {
                .directional => |dir_light| {
                    light.pos = dir_light.dir.toVec4(0);
                    light.color_radius = dir_light.color.toVec4(0);
                    gl.viewport(0, 0, DIRECTIONAL_SHADOW_MAP_SIZE, DIRECTIONAL_SHADOW_MAP_SIZE);

                    const camera_matrix = &self.shadow_matrices;

                    const view = Mat4.lookAt(
                        dir_light.dir.scale(-1),
                        Vec3.zero(),
                        Vec3.up(),
                    );

                    const shadow_map_idx = 0;

                    light.view_mat = view;
                    light.params.shadow_map_idx = shadow_map_idx;
                    light.params.csm_split_count = @floatFromInt(CSM_SPLITS);

                    var splits: [CSM_SPLITS + 1]f32 = undefined;

                    const splits_count_f: f32 = @floatFromInt(CSM_SPLITS);
                    for (0..CSM_SPLITS + 1) |split_idx| {
                        const split_idx_f: f32 = @floatFromInt(split_idx);
                        const split_i_over_n = split_idx_f / splits_count_f;
                        const expo_split = self.camera.near * std.math.pow(f32, self.camera.far / self.camera.near, split_i_over_n);
                        const uniform_split = self.camera.near + split_i_over_n * (self.camera.far - self.camera.near);
                        const split = CSM_EXPO_UNIFORM_FACTOR * expo_split + (1.0 - CSM_EXPO_UNIFORM_FACTOR) * uniform_split;
                        splits[split_idx] = split;
                    }

                    for (0..CSM_SPLITS) |split_idx| {
                        const split_near = splits[split_idx];
                        const split_far = splits[split_idx + 1];

                        gl.namedFramebufferTextureLayer(self.shadow_framebuffer, gl.DEPTH_ATTACHMENT, self.shadow_texture_array, 0, @intCast(shadow_map_idx * CSM_SPLITS + split_idx));
                        const check_fbo_status = gl.checkNamedFramebufferStatus(self.shadow_framebuffer, gl.DRAW_FRAMEBUFFER);
                        if (check_fbo_status != gl.FRAMEBUFFER_COMPLETE) {
                            std.log.debug("Shadow Framebuffer Incomplete: {}\n", .{check_fbo_status});
                        }

                        var projection: Mat4 = undefined;

                        {
                            var camera = self.camera.*;
                            if (self.update_view_frustum) {
                                camera.near = split_near;
                                camera.far = split_far;
                                const inv_csm_proj = camera.projection().mul(camera.view_mat).inv();

                                for (math.ndc_box_corners, 0..) |corner, corner_idx| {
                                    const pos4 = inv_csm_proj.mulByVec4(corner.toVec4(1));
                                    self.world_view_frustum_corners[split_idx][corner_idx] = pos4.toVec3().scale(1.0 / pos4.w());
                                }
                            }

                            // Find minimal bounding sphere for a frustum
                            // Taken from:
                            // https://lxjk.github.io/2017/04/15/Calculate-Minimal-Bounding-Sphere-of-Frustum.html
                            const inv_aspect_sqr = (camera.aspect) * (camera.aspect);
                            const k = @sqrt(1 + inv_aspect_sqr) * @tan(za.toRadians(camera.fovy) / 2);

                            var center = Vec3.zero();
                            var radius: f32 = 0;
                            if (k * k >= (camera.far - camera.near) / (camera.far + camera.near)) {
                                center = Vec3.new(0, 0, -camera.far);
                                radius = camera.far * k;
                            } else {
                                center = Vec3.new(0, 0, -0.5 * (camera.far + camera.near) * (1 + k * k));
                                radius = 0.5 * @sqrt((camera.far - camera.near) * (camera.far - camera.near) + 2 * (camera.far * camera.far + camera.near * camera.near) * k * k + (camera.far + camera.near) * (camera.far + camera.near) * k * k * k * k);
                            }

                            center = camera.view_mat.inv().mulByVec4(center.toVec4(1)).toVec3();
                            center = view.mulByVec4(center.toVec4(1)).toVec3();

                            // NOTE: Use bounding sphere instead of AABB to prevent split size changing with rotation
                            projection = math.orthographic(
                                center.x() - radius - 0.0001,
                                center.x() + radius,
                                center.y() - radius,
                                center.y() + radius,
                                -center.z() - radius,
                                -center.z() + radius,
                            );
                        }

                        var shadow_view_proj = projection.mul(view);

                        // Snap to texels
                        {
                            var shadow_origin = shadow_view_proj.mulByVec4(Vec4.new(0, 0, 0, 1));
                            shadow_origin = shadow_origin.scale(1.0 / shadow_origin.w());
                            shadow_origin = shadow_origin.scale(DIRECTIONAL_SHADOW_MAP_SIZE / 2);
                            var rounded_origin: Vec4 = undefined;
                            rounded_origin.data = @round(shadow_origin.data);
                            var offset = rounded_origin.sub(shadow_origin).toVec2().toVec3(0);
                            offset = offset.scale(2.0 / @as(f32, DIRECTIONAL_SHADOW_MAP_SIZE));
                            projection = projection.translate(offset);
                            shadow_view_proj = projection.mul(view);
                        }

                        camera_matrix.* = .{
                            .view = view,
                            .projection = projection,
                        };
                        dir_view_proj_mat[split_idx] = shadow_view_proj;
                        const light_frustum = math.Frustum.new(shadow_view_proj);

                        light.view_proj_mats[split_idx] = shadow_view_proj;
                        light.csm_split_points[split_idx] = -split_far;

                        gl.namedBufferSubData(self.shadow_matrices_buffer, 0, @sizeOf(CameraMatrices), std.mem.asBytes(&self.shadow_matrices));
                        checkGLError();

                        gl.clear(gl.DEPTH_BUFFER_BIT);
                        gl.bindBufferBase(gl.UNIFORM_BUFFER, UBO.CameraMatrices.value(), self.shadow_matrices_buffer);

                        self.renderShadow(&light_frustum);
                    }
                },
                .point => |point_light| {
                    if (!finished_dir_lights) {
                        finished_dir_lights = true;
                        gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.cube_shadow).program);
                    }

                    const pos = point_light.pos;
                    light.pos = pos.toVec4(1);
                    light.color_radius = point_light.color.toVec4(point_light.radius);

                    const range = pointLightRange(&point_light);
                    const near_far = Vec2.new(0.1, range);

                    light.view_mat = Mat4.fromTranslate(pos.negate());
                    light.params.near = near_far.x();
                    light.params.far = near_far.y();

                    const shadow_map_idx = i;
                    light.params.shadow_map_idx = @floatFromInt(shadow_map_idx);

                    // For each cube face
                    for (cube_camera_dirs, 0..) |cam_dir, face| {
                        gl.namedFramebufferTextureLayer(self.shadow_framebuffer, gl.DEPTH_ATTACHMENT, self.cube_shadow_texture_array, 0, @intCast(shadow_map_idx * 6 + face));
                        const check_fbo_status = gl.checkNamedFramebufferStatus(self.shadow_framebuffer, gl.DRAW_FRAMEBUFFER);
                        if (check_fbo_status != gl.FRAMEBUFFER_COMPLETE) {
                            std.log.debug("Shadow Framebuffer Incomplete: {}\n", .{check_fbo_status});
                        }

                        gl.viewport(0, 0, 512, 512);

                        const camera_matrix = &self.shadow_matrices;
                        camera_matrix.* = .{
                            .projection = math.perspective(90, 1, near_far.x(), near_far.y()),
                            .view = Mat4.lookAt(
                                pos,
                                pos.add(cam_dir.target),
                                cam_dir.up,
                            ),
                        };

                        const shadow_view_proj = camera_matrix.projection.mul(camera_matrix.view);
                        const light_frustum = math.Frustum.new(shadow_view_proj);
                        gl.uniform2f(Uniform.NearFarPlanes.value(), near_far.x(), near_far.y());

                        gl.namedBufferSubData(self.shadow_matrices_buffer, 0, @sizeOf(CameraMatrices), std.mem.asBytes(&self.shadow_matrices));
                        checkGLError();

                        gl.clear(gl.DEPTH_BUFFER_BIT);
                        gl.bindBufferBase(gl.UNIFORM_BUFFER, UBO.CameraMatrices.value(), self.shadow_matrices_buffer);

                        self.renderShadow(&light_frustum);
                    }
                },
            }
        }
    }

    // Light world space to view space
    for (lights_buf.data[0..lights_buf.count.*]) |*light| {
        light.pos = self.camera.view_mat.mulByVec4(light.pos);
    }

    var width: c_int = 0;
    var height: c_int = 0;
    c.SDL_GL_GetDrawableSize(globals.g_init.window, &width, &height);

    if (width != self.screen_tex_size.x() or height != self.screen_tex_size.y()) {
        self.updateScreenBufferSize(width, height);
    }

    gl.namedFramebufferTexture(self.screen_fbo, gl.COLOR_ATTACHMENT0, self.screen_color_texture, 0);
    gl.namedFramebufferTexture(self.screen_fbo, gl.DEPTH_ATTACHMENT, self.screen_depth_texture, 0);

    if (gl.checkNamedFramebufferStatus(self.screen_fbo, gl.DRAW_FRAMEBUFFER) != gl.FRAMEBUFFER_COMPLETE) {
        checkGLError();
        @panic("Framebuffer incomplete");
    }

    gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, self.screen_fbo);

    gl.viewport(0, 0, width, height);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    const switched_to_alpha_blend = false;
    gl.disable(gl.BLEND);

    const draw_indirect_cmds_c: [*]u8 = @ptrCast(gl.mapNamedBuffer(
        self.draw_indirect_buffer,
        gl.WRITE_ONLY,
    ) orelse {
        checkGLError();
        @panic("map draw indirect buffer");
    });
    var draw_indirect_cmds = std.mem.bytesAsSlice(DrawIndirectCmd, draw_indirect_cmds_c[0 .. @sizeOf(DrawIndirectCmd) * MAX_DRAW_COMMANDS]);

    const materials = self.materials_pbr_ssbo.getInstance(self.tripple_buffer_index);
    materials.count.* = 0;

    const draw_cmd_data = self.draw_cmd_data_ssbo.getInstance(self.tripple_buffer_index);

    var rendered_count: usize = 0;
    cmds: for (self.command_buffer[0..self.command_count]) |*cmd| {
        const mesh = self.assetman.resolveMesh(cmd.mesh);
        const aabb = math.AABB.fromMinMax(mesh.aabb.min, mesh.aabb.max);

        if (!self.world_camera_frustum.intersectAABB(aabb.transform(cmd.transform))) {
            continue;
        }

        const material: Material = if (cmd.material_override) |mat| mat else mesh.material;

        // Opaque objects are drawn, start rendering alpha blended objects
        if (material.blend_mode == .AlphaBlend and !switched_to_alpha_blend) {
            break :cmds;
            // switched_to_alpha_blend = true;
            // gl.enable(gl.BLEND);
            // gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
        }

        draw_cmd_data.data[rendered_count] = DrawCommandData{
            .transform = cmd.transform,
        };

        materials.data[rendered_count] = MaterialPBR.fromMaterial(self.assetman, &material);
        materials.count.* += 1;

        draw_indirect_cmds[rendered_count] = DrawIndirectCmd{
            .count = mesh.indices.count,
            .instance_count = 1,
            .first_index = mesh.indices.offset / 4,
            .base_vertex = mesh.indices.base_vertex,
            .base_instance = 0,
            .transform = cmd.transform,
        };

        rendered_count += 1;
    }

    _ = gl.unmapNamedBuffer(self.draw_indirect_buffer);

    gl.bindBuffer(gl.DRAW_INDIRECT_BUFFER, self.draw_indirect_buffer);

    {
        const camera_matrix: *CameraMatrices = @alignCast(@ptrCast(self.camera_matrices[self.tripple_buffer_index * self.uboAlignedSizeOf(CameraMatrices) ..].ptr));
        camera_matrix.* = .{
            .projection = camera_projection,
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

    gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.z_prepass).program);
    gl.bindVertexArray(self.shadow_vao);

    self.assetman.vertex_heap.vertices.bind(Render.Attrib.Position.value());
    checkGLError();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, self.assetman.vertex_heap.indices.buffer);
    checkGLError();

    gl.multiDrawElementsIndirect(gl.TRIANGLES, gl.UNSIGNED_INT, null, @intCast(rendered_count), @sizeOf(DrawIndirectCmd));

    gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.mesh).program);
    gl.bindVertexArray(self.mesh_vao);
    gl.depthFunc(gl.LEQUAL);

    gl.uniform1ui(Uniform.LightsCount.value(), lights_buf.count.*);
    gl.GL_ARB_bindless_texture.uniformHandleui64ARB(Uniform.ShadowMap2D.value(), self.shadow_texture_handle);
    gl.GL_ARB_bindless_texture.uniformHandleui64ARB(Uniform.ShadowMapCube.value(), self.cube_shadow_texture_handle);

    self.assetman.vertex_heap.vertices.bind(Render.Attrib.Position.value(), 0);
    checkGLError();
    self.assetman.vertex_heap.ps_data.bind(Render.Attrib.Normal.value(), @offsetOf(formats.VertexPSData, "normal"));
    checkGLError();
    self.assetman.vertex_heap.ps_data.bind(Render.Attrib.Tangent.value(), @offsetOf(formats.VertexPSData, "tangent"));
    checkGLError();
    self.assetman.vertex_heap.ps_data.bind(Render.Attrib.UV.value(), @offsetOf(formats.VertexPSData, "uv"));
    checkGLError();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, self.assetman.vertex_heap.indices.buffer);
    checkGLError();

    gl.multiDrawElementsIndirect(gl.TRIANGLES, gl.UNSIGNED_INT, null, @intCast(rendered_count), @sizeOf(DrawIndirectCmd));

    gl.disable(gl.BLEND);
    gl.depthFunc(gl.LESS);

    // Debug stuff
    {
        gl.polygonMode(gl.FRONT_AND_BACK, gl.LINE);
        defer gl.polygonMode(gl.FRONT_AND_BACK, gl.FILL);
        gl.lineWidth(4);

        // Frustum debug stuff, drawn only when view frustum is fixed
        if (!self.update_view_frustum) {
            gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.unlit).program);

            // Draw wire frustum cubes
            {
                const mesh = self.assetman.resolveMesh(a.Meshes.cube.Cube);
                mesh.positions.bind(Render.Attrib.Position.value());
                gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);
                gl.uniform3fv(Uniform.Color.value(), 1, @ptrCast(&Vec3.one().data));

                const model = Mat4.fromTranslate(Vec3.new(0, 0, 0.5)).mul(Mat4.fromScale(Vec3.new(1, 1, 0.5)));

                var view_proj_matrices: [1 + CSM_SPLITS]Mat4 = undefined;
                for (0..CSM_SPLITS) |split_idx| {
                    view_proj_matrices[split_idx] = dir_view_proj_mat[split_idx];
                }
                view_proj_matrices[CSM_SPLITS] = self.camera_view_proj;

                for (view_proj_matrices) |frustum_view_proj| {
                    const frustum_model_mat = frustum_view_proj.inv().mul(model);
                    gl.uniformMatrix4fv(Uniform.ModelMatrix.value(), 1, gl.FALSE, @ptrCast(&frustum_model_mat.data));
                    gl.drawElementsBaseVertex(
                        gl.TRIANGLES,
                        @intCast(mesh.indices.count),
                        mesh.indices.type,
                        @ptrFromInt(mesh.indices.offset),
                        mesh.indices.base_vertex,
                    );
                }
            }
            // Draw corner positions of view frustum
            {
                const mesh = self.assetman.resolveMesh(a.Meshes.sphere.Icosphere);
                mesh.positions.bind(Attrib.Position.value());
                mesh.indices.bind();

                gl.uniform3fv(Uniform.Color.value(), 1, @ptrCast(&Vec3.new(1, 0, 0).data));

                for (0..CSM_SPLITS) |split_idx| {
                    for (self.world_view_frustum_corners[split_idx]) |corner| {
                        const model = Mat4.fromTranslate(corner);
                        gl.uniformMatrix4fv(Uniform.ModelMatrix.value(), 1, gl.FALSE, @ptrCast(&model.data));
                        gl.drawElementsBaseVertex(gl.TRIANGLES, @intCast(mesh.indices.count), mesh.indices.type, @ptrFromInt(mesh.indices.offset), mesh.indices.base_vertex);
                    }
                }
            }
        }
    }

    //std.log.debug("Total draws {}, frustum culled draws {}\n", .{ self.command_count, rendered_count });

    gl.disable(gl.DEPTH_TEST);
    gl.bindVertexArray(self.post_process_vao); // shared for all post process shaders

    const quad = self.assetman.resolveMesh(a.Meshes.quad.Plane);
    // Bind quad
    {
        quad.positions.bind(Render.Attrib.Position.value());
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, quad.indices.buffer);
    }

    // Bloom pass
    {
        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, self.screen_fbo);

        gl.bindTextureUnit(0, self.screen_color_texture);
        gl.bindSampler(0, self.screen_bloom_sampler);
        defer gl.bindSampler(0, 0);

        // Downsample and filter
        {
            gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.bloom_downsample).program);

            for (1..self.screen_mip_count) |dst_mip_level| {
                const src_mip_level = dst_mip_level - 1;
                gl.namedFramebufferTexture(self.screen_fbo, gl.COLOR_ATTACHMENT0, self.screen_color_texture, @intCast(dst_mip_level));
                const size = getMipSize(self.screen_tex_size.x(), self.screen_tex_size.y(), dst_mip_level);
                gl.viewport(0, 0, size.x(), size.y());
                gl.uniform1i(Uniform.SRCMipLevel.value(), @intCast(src_mip_level));

                gl.drawElementsBaseVertex(
                    gl.TRIANGLES,
                    @intCast(quad.indices.count),
                    quad.indices.type,
                    @ptrFromInt(quad.indices.offset),
                    quad.indices.base_vertex,
                );
            }
        }

        // Upsample
        {
            gl.enable(gl.BLEND);
            defer gl.disable(gl.BLEND);
            gl.blendFunc(gl.ONE, gl.ONE);

            gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.bloom_upsample).program);

            var src_mip_level = self.screen_mip_count - 1;
            while (src_mip_level > 0) : (src_mip_level -= 1) {
                const dst_mip_level = src_mip_level - 1;
                gl.namedFramebufferTexture(self.screen_fbo, gl.COLOR_ATTACHMENT0, self.screen_color_texture, @intCast(dst_mip_level));
                const size = getMipSize(self.screen_tex_size.x(), self.screen_tex_size.y(), dst_mip_level);
                gl.viewport(0, 0, size.x(), size.y());
                gl.uniform1i(Uniform.SRCMipLevel.value(), @intCast(src_mip_level));
                gl.uniform1f(Uniform.BloomStrength.value(), if (dst_mip_level == 0) 0.04 else 1);

                gl.drawElementsBaseVertex(
                    gl.TRIANGLES,
                    @intCast(quad.indices.count),
                    quad.indices.type,
                    @ptrFromInt(quad.indices.offset),
                    quad.indices.base_vertex,
                );
            }
        }
    }

    // Final post processing pass
    {
        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, 0);
        //gl.clear(gl.DEPTH_BUFFER_BIT | gl.COLOR_BUFFER_BIT);
        gl.viewport(0, 0, width, height);

        gl.useProgram(self.assetman.resolveShaderProgram(a.ShaderPrograms.shaders.post_process).program);

        gl.bindTextureUnit(0, self.screen_color_texture);
        defer gl.bindTextureUnit(0, 0);

        gl.drawElementsBaseVertex(gl.TRIANGLES, @intCast(quad.indices.count), quad.indices.type, @ptrFromInt(quad.indices.offset), quad.indices.base_vertex);
    }

    self.gl_fences[self.tripple_buffer_index] = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    c.SDL_GL_SwapWindow(ginit.window);
    //c.SDL_Delay(1);
}

pub fn pointLightRange(self: *const PointLight) f32 {
    const color = self.color;
    const light_intensity = @max(color.x(), color.y(), color.z());

    const cutoff = 0.005;
    return self.radius * (@sqrt(light_intensity / cutoff) - 1);
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

fn renderShadow(self: *Render, frustum: *const math.Frustum) void {
    for (self.command_buffer[0..self.command_count]) |*cmd| {
        const mesh = self.assetman.resolveMesh(cmd.mesh);
        const aabb = math.AABB.fromMinMax(mesh.aabb.min, mesh.aabb.max);

        if (!frustum.intersectAABBSkipNear(aabb.transform(cmd.transform))) {
            continue;
        }

        gl.uniformMatrix4fv(Uniform.ModelMatrix.value(), 1, gl.FALSE, @ptrCast(&cmd.transform.data));
        mesh.positions.bind(Render.Attrib.Position.value());
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);

        gl.drawElementsBaseVertex(
            gl.TRIANGLES,
            @intCast(mesh.indices.count),
            mesh.indices.type,
            @ptrFromInt(mesh.indices.offset),
            mesh.indices.base_vertex,
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

    pub inline fn value(self: UBO) gl.GLuint {
        return @intFromEnum(self);
    }
};
pub const SSBO = enum(gl.GLuint) {
    PointLights = 1,
    Materials = 2,
    DrawCommandData = 3,

    pub inline fn value(self: SSBO) gl.GLuint {
        return @intFromEnum(self);
    }
};

pub fn getStd430Align(comptime T: type) usize {
    switch (T) {
        Vec2, Vec2_i32 => {
            return 8;
        },
        Vec3, Vec4 => {
            return 16;
        },
        Mat4 => {
            return 16;
        },
    }

    const info = @typeInfo(T);
    switch (info) {
        .Int => |int| {
            if (int.bits & (int.bits - 1) != 0) {
                @compileError("Non power of two bit size of int");
            }
            const byte_size = int.bits / 8;

            return @intCast(byte_size);
        },
        .Float => |float| {
            if (float.bits & (float.bits - 1) != 0) {
                @compileError("Non power of two bit size of float");
            }
            const byte_size = float.bits / 8;

            return @intCast(byte_size);
        },
        .Struct => |str| {
            if (str.layout != .@"extern") {
                @compileError("Structs should be extern for std430");
            }

            // inline for (str.fields) |field| {
            //     field.
            // }
            return 0;
        },
        _ => @compileError("Unknown type for std430 " ++ @typeName(T)),
    }
}

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

    // Bloom
    SRCMipLevel = 19,
    BloomStrength = 20,
    LightsCount = 21,

    pub inline fn value(self: Uniform) gl.GLint {
        return @intFromEnum(self);
    }
};

// TODO: support ortho
pub const Camera = struct {
    pos: Vec3 = Vec3.zero(),

    fovy: f32 = 60,
    aspect: f32 = 1,
    near: f32 = 0.1,
    far: f32 = 10,

    view_mat: Mat4 = Mat4.identity(),

    pub fn projection(self: *const Camera) Mat4 {
        return math.perspective(self.fovy, self.aspect, self.near, self.far);
    }
};

// Should be std140
const CameraMatrices = extern struct {
    projection: Mat4 = Mat4.identity(),
    view: Mat4 = Mat4.identity(),
};
pub const Light = extern struct {
    pos: Vec4, // x, y, z, w - vPos
    color_radius: Vec4, // x, y, z - color, w - radius
    view_mat: Mat4 = Mat4.identity(),

    // for directional lights contains view projection matrices for each split
    // TODO: comprejk   ss this somehow
    view_proj_mats: [4]Mat4 = undefined,

    // Usese floats because it's a vec4 on the other end
    params: extern struct {
        near: f32,
        far: f32,
        shadow_map_idx: f32,
        csm_split_count: f32,
    },
    csm_split_points: [4]f32 = undefined,

    /// Alignment of this struct if it was in a std430 array
    pub fn alignStd430() usize {
        return @alignOf(Light);
    }

    /// Aligned size of this struct if it was in a std430 array
    pub fn sizeOfStd430() usize {
        return @sizeOf(Light);
        // return std.mem.alignForward(usize, @sizeOf(Light), Light.alignStd430());
    }
};

const LightSSBO = BufferSSBOAlign(Light, 16);

// Shader struct for material data
pub const MaterialPBR = extern struct {
    albedo: Vec4,
    albedo_map: gl.GLuint64,
    albedo_map_uv_scale: Vec2,
    normal_map: gl.GLuint64,
    normal_map_uv_scale: Vec2,
    metallic: f32,
    metallic_map: gl.GLuint64,
    metallic_map_uv_scale: Vec2,
    roughness: f32,
    roughness_map: gl.GLuint64,
    roughness_map_uv_scale: Vec2,
    emission: Vec3 align(16),
    emission_map: gl.GLuint64,
    emission_map_uv_scale: Vec2,

    pub fn fromMaterial(assetman: *AssetManager, mat: *const Material) MaterialPBR {
        const albedo_map = assetman.resolveTexture(mat.albedo_map);
        const normal_map = assetman.resolveTexture(mat.normal_map);
        const metallic_map = assetman.resolveTexture(mat.metallic_map);
        const roughness_map = assetman.resolveTexture(mat.roughness_map);
        const emission_map = assetman.resolveTexture(mat.emission_map);
        return .{
            .albedo = mat.albedo,
            .albedo_map = albedo_map.handle,
            .albedo_map_uv_scale = albedo_map.uv_scale,
            .normal_map = normal_map.handle,
            .normal_map_uv_scale = normal_map.uv_scale,
            .metallic = mat.metallic,
            .metallic_map = metallic_map.handle,
            .metallic_map_uv_scale = metallic_map.uv_scale,
            .roughness = mat.roughness,
            .roughness_map = roughness_map.handle,
            .roughness_map_uv_scale = roughness_map.uv_scale,
            .emission = mat.emission,
            .emission_map = emission_map.handle,
            .emission_map_uv_scale = emission_map.uv_scale,
        };
    }

    /// Alignment of this struct if it was in a std430 array
    pub fn alignStd430() usize {
        return @alignOf(MaterialPBR);
    }

    /// Aligned size of this struct if it was in a std430 array
    pub fn sizeOfStd430() usize {
        return @sizeOf(MaterialPBR);
        //return std.mem.alignForward(usize, @sizeOf(MaterialPBR), MaterialPBR.alignStd430());
    }
};

pub fn BufferSSBO(comptime T: type) type {
    return BufferSSBOAlign(T, @alignOf(T));
}

// Helper struct for using ssbo arrays with count
// It provides a coherent always mapped buffer
pub fn BufferSSBOAlign(comptime T: type, comptime alignment: usize) type {
    switch (@typeInfo(T)) {
        .Struct => |str| {
            if (str.layout != .@"extern") {
                @compileError("Use extern layout for SSBO structs");
            }
        },
        else => {},
    }

    return struct {
        pub const BufferInstance = struct {
            count: *c_uint,
            data: []T align(alignment),
        };

        // Helper struct to calculate buffer sizes
        // not actually used
        const BufferLayout = extern struct {
            count: c_uint,
            _start: [0]T align(alignment),

            pub fn calculateBufSize(max_count: usize, ssbo_align: usize) usize {
                return std.mem.alignForward(usize, @sizeOf(BufferInstance) + @sizeOf(T) * max_count, ssbo_align);
            }

            pub fn getData(self: *BufferLayout, len: usize) []T {
                var data_c: [*]T = @ptrFromInt(@intFromPtr(self) + @offsetOf(BufferLayout, "_start"));

                return data_c[0..len];
            }
        };

        const Self = @This();

        len: usize = 0,
        /// How many buffer instances of length `len` are in a single GL buffer
        len_buffers: usize = 0,
        buffer: gl.GLuint = 0,
        data: []u8 = &.{},

        // Don't like duplicating it here, but don't have a better idea
        ssbo_align: usize = 0,

        pub fn init(ssbo_align: usize, len: usize, num_buffers: usize) !Self {
            var result = Self{
                .len = len,
                .len_buffers = num_buffers,
                .ssbo_align = ssbo_align,
            };

            gl.createBuffers(1, &result.buffer);
            if (result.buffer == 0) {
                checkGLError();
                return error.CreateBuffers;
            }

            const PERSISTENT_BUFFER_FLAGS: gl.GLbitfield = gl.MAP_PERSISTENT_BIT | gl.MAP_WRITE_BIT | gl.MAP_COHERENT_BIT;

            const buf_size = BufferLayout.calculateBufSize(len, ssbo_align) * num_buffers;
            gl.namedBufferStorage(
                result.buffer,
                @intCast(buf_size),
                null,
                PERSISTENT_BUFFER_FLAGS,
            );
            const data_c: [*]u8 = @ptrCast(gl.mapNamedBufferRange(
                result.buffer,
                0,
                @intCast(buf_size),
                PERSISTENT_BUFFER_FLAGS,
            ) orelse {
                checkGLError();
                @panic("bind point_lights_ssbo");
            });

            result.data = data_c[0..buf_size];

            return result;
        }

        pub fn deinit(self: *Self) void {
            gl.deleteBuffers(1, &self.buffer);
            self.buffer = 0;
            self.data = &.{};
        }

        pub fn getInstance(self: *Self, index: usize) BufferInstance {
            std.debug.assert(index < self.len_buffers);

            const layout: *BufferLayout = @alignCast(@ptrCast(self.data[index * BufferLayout.calculateBufSize(self.len, self.ssbo_align) ..].ptr));

            return BufferInstance{
                .count = &layout.count,
                .data = layout.getData(self.len),
            };
        }

        pub fn bind(self: *const Self, idx: usize, binding: SSBO) void {
            std.debug.assert(idx < self.len_buffers);

            const size = BufferLayout.calculateBufSize(self.len, self.ssbo_align);
            gl.bindBufferRange(
                gl.SHADER_STORAGE_BUFFER,
                binding.value(),
                self.buffer,
                idx * size,
                @intCast(size),
            );
        }
    };
}

const MaterialPBRSSBO = BufferSSBO(MaterialPBR);

const DrawCommandData = extern struct {
    transform: Mat4,
};

const DrawCommandDataSSBO = BufferSSBOAlign(DrawCommandData, 16);

const DrawIndirectCmd = extern struct {
    count: gl.GLuint,
    instance_count: gl.GLuint,
    first_index: gl.GLuint,
    base_vertex: gl.GLint,
    base_instance: gl.GLuint,
    transform: Mat4,
};

fn uboAlignedSizeOf(self: *const Render, comptime T: type) usize {
    return std.mem.alignForward(usize, @sizeOf(T), self.ubo_align);
}

fn ssboAlign(self: *const Render, size: usize) usize {
    return std.mem.alignForward(usize, size, self.ssbo_align);
}

fn ssboAlignedSizeOf(self: *const Render, comptime T: type) usize {
    return self.ssboAlign(@sizeOf(T));
}
