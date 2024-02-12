const std = @import("std");
const c = @import("sdl.zig");
const manymouse = @import("manymouse.zig");
const gl = @import("gl.zig");
const AssetManager = @import("AssetManager.zig");
const formats = @import("formats.zig");
const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;
const a = @import("asset_manifest");
const windows = std.os.windows;

pub extern "dwmapi" fn DwmEnableMMCSS(fEnableMMCSS: windows.BOOL) callconv(windows.WINAPI) windows.HRESULT;
pub extern "dwmapi" fn DwmFlush() callconv(windows.WINAPI) void;

const D3DKMT_HANDLE = c_uint;
const D3DDDI_VIDEO_PRESENT_SOURCE_ID = c_uint;

const D3DKMT_WAITFORVERTICALBLANKEVENT = extern struct {
    hAdapter: D3DKMT_HANDLE,
    hDevice: D3DKMT_HANDLE,
    VidPnSourceId: D3DDDI_VIDEO_PRESENT_SOURCE_ID,
};
pub extern "gdi32" fn D3DKMTWaitForVerticalBlankEvent(event: *const D3DKMT_WAITFORVERTICALBLANKEVENT) callconv(windows.WINAPI) windows.NTSTATUS;

const FRAME_ARENA_SIZE = 1024 * 1024 * 512;

const MAX_FRAMES_QUEUED = 3;

const DEFAULT_WIDTH = 800;
const DEFAULT_HEIGHT = 600;

const MAX_ENTITIES = 1024;
const MAX_POINT_LIGHTS = 8;

// TODO: move out into renderer file maybe
const Attrib = enum(gl.GLuint) {
    Position = 0,
    Normal = 1,
    UV = 2,

    pub inline fn value(self: Attrib) gl.GLuint {
        return @intFromEnum(self);
    }
};
const UBO = enum(gl.GLuint) {
    CameraMatrices = 0,
    PointLights = 1,

    pub inline fn value(self: UBO) gl.GLuint {
        return @intFromEnum(self);
    }
};

pub const InitMemory = struct {
    global_allocator: std.mem.Allocator,
    window: *c.SDL_Window,
    context: ?*anyopaque,
    width: c_int,
    height: c_int,
    syswm_info: c.SDL_SysWMinfo = .{},
};

pub const Entity = struct {
    pub const Flags = packed struct {
        active: bool = false,
        mesh: bool = false,
        point_light: bool = false,
        rotate: bool = false,
    };

    pub const Transform = struct {
        pos: Vec3 = Vec3.zero(),
        rot: Quat = Quat.identity(),
        scale: Vec3 = Vec3.one(),

        pub fn matrix(self: *Transform) Mat4 {
            // TODO: cache
            return Mat4.recompose(self.pos, self.rot, self.scale);
        }
    };

    pub const Mesh = struct {
        handle: AssetManager.Handle.Mesh = .{},
        color: Vec3 = Vec3.one(),
    };
    pub const PointLight = struct {
        radius: f32 = std.math.floatEps(f32), // should never be 0 or bad things happen
        color_intensity: Vec4 = Vec4.one(), // x, y, z - color, w - intensity

        pub fn color(self: *PointLight) Vec3 {
            const col = self.color_intensity;
            return Vec3.new(col.x(), col.y(), col.z());
        }
    };
    pub const Rotate = struct {
        axis: Vec3 = Vec3.up(),
        rate: f32 = 0, // deg/s
    };

    // Entity list and handle management
    idx: u32 = 0,
    gen: u32 = 0,
    // Free list
    next: ?*Entity = null,

    flags: Flags = .{},
    transform: Transform = .{},
    mesh: Mesh = .{},
    point_light: PointLight = .{},
    rotate: Rotate = .{},
};

pub const EntityHandle = packed struct {
    idx: u32,
    gen: u32,
};

pub const World = struct {
    entities: [MAX_ENTITIES]Entity = [_]Entity{.{}} ** MAX_ENTITIES,
    entity_count: usize = 0,
    free_entity: ?*Entity = null,

    pub fn addEntity(self: *World, entity: Entity) EntityHandle {
        const ent = result: {
            if (self.free_entity) |ent| {
                break :result ent;
            } else {
                const new_entity = &self.entities[self.entity_count];
                new_entity.idx = @intCast(self.entity_count);
                self.entity_count += 1;
                break :result new_entity;
            }
        };
        const next = ent.next;
        const gen = ent.gen;
        const idx = ent.idx;
        ent.* = entity;
        ent.gen = gen + 1;
        ent.idx = idx;
        ent.flags.active = true;
        self.free_entity = next;

        return EntityHandle{ .idx = idx, .gen = ent.gen };
    }

    pub fn getEntity(self: *World, handle: EntityHandle) ?*Entity {
        const ent = &self.entities[handle.idx];
        if (ent.gen != handle.gen) {
            return null;
        }
        return ent;
    }

    pub fn removeEntity(self: *World, handle: EntityHandle) void {
        const ent = self.getEntity(handle) orelse return;
        ent.flags.active = false;
        ent.next = self.free_entity;
        self.free_entity = ent;
    }
};

pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    frame_fba: std.heap.FixedBufferAllocator,
    assetman: AssetManager,
    performance_frequency: u64 = 0,
    last_frame_time: u64 = 0,
    delta_time: f32 = 0.0000001,
    mesh_vao: gl.GLuint = 0,
    tripple_buffer_index: usize = MAX_FRAMES_QUEUED - 1,
    gl_fences: [MAX_FRAMES_QUEUED]?gl.GLsync = [_]?gl.GLsync{null} ** MAX_FRAMES_QUEUED,
    camera_ubo: gl.GLuint = 0,
    camera_matrices: []CameraMatrices = &.{},
    point_lights_ubo: gl.GLuint = 0,
    point_lights: []RenderPointLightArray = &.{},
    rotation: f32 = 0,
    input_state: InputState = .{},
    free_cam: FreeLookCamera = .{},
    mouse_focus: bool = false,
    world: World = .{},
};

pub const InputState = packed struct {
    left: bool = false,
    right: bool = false,
    forward: bool = false,
    backward: bool = false,
    up: bool = false,
    down: bool = false,
};

pub const FreeLookCamera = struct {
    pos: Vec3 = Vec3.new(0, 0, -1),
    pitch: f32 = 0,
    yaw: f32 = 0,
    move_speed: f32 = 0.5,
    view_matrix: Mat4 = Mat4.identity(),

    pub fn update(self: *FreeLookCamera, dt: f32, move: Vec3, look: Vec2) void {
        self.yaw += look.x();
        self.pitch += look.y();
        // First rotate pitch, then yaw
        const rot = Mat4.fromRotation(self.pitch, Vec3.left()).mul(Mat4.fromRotation(self.yaw, Vec3.up()));

        // First 3 of transform matrix are: right, up, forward
        const left = Vec3.new(rot.data[0][0], rot.data[1][0], rot.data[2][0]);
        const up = Vec3.new(rot.data[0][1], rot.data[1][1], rot.data[2][1]);
        const forward = Vec3.new(rot.data[0][2], rot.data[1][2], rot.data[2][2]);

        const movement = left.scale(-move.x()).add(forward.scale(move.y())).add(up.scale(move.z()));

        self.pos = self.pos.add(movement.scale(self.move_speed * dt));

        self.view_matrix = Mat4.lookAt(self.pos, self.pos.add(forward), Vec3.up());
    }
};

var g_init_exists = false;
var g_init: *InitMemory = undefined;
var g_mem: *GameMemory = undefined;
var g_assetman: *AssetManager = undefined;

fn game_init_window_err(global_allocator: std.mem.Allocator) !void {
    // _ = DwmEnableMMCSS(1);
    try sdl_try(c.SDL_Init(c.SDL_INIT_EVERYTHING));

    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_DOUBLEBUFFER, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, 4));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, 5));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, c.SDL_GL_CONTEXT_PROFILE_CORE));

    const maybe_window = c.SDL_CreateWindow(
        "Learn OpenGL with Zig!",
        c.SDL_WINDOWPOS_UNDEFINED,
        c.SDL_WINDOWPOS_UNDEFINED,
        DEFAULT_WIDTH,
        DEFAULT_HEIGHT,
        c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_ALLOW_HIGHDPI | c.SDL_WINDOW_RESIZABLE,
    );
    if (maybe_window == null) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLWindowError;
    }
    const window = maybe_window.?;

    const context = c.SDL_GL_CreateContext(window);

    _ = c.SDL_GL_SetSwapInterval(0);

    g_init = try global_allocator.create(InitMemory);
    g_init_exists = true;
    g_init.* = .{
        .global_allocator = global_allocator,
        .window = window,
        .context = context,
        .width = DEFAULT_WIDTH,
        .height = DEFAULT_HEIGHT,
    };

    const version = &g_init.syswm_info.version;
    version.major = c.SDL_MAJOR_VERSION;
    version.minor = c.SDL_MINOR_VERSION;
    version.patch = c.SDL_PATCHLEVEL;

    if (c.SDL_GetWindowWMInfo(window, &g_init.syswm_info) == 0) {
        const err = c.SDL_GetError();
        std.log.err("Failed to get syswm info: {s}", .{err});
        return error.SDLSysWMInfo;
    }
}

export fn game_init_window(global_allocator: *std.mem.Allocator) void {
    std.log.debug("game_init_window\n", .{});
    game_init_window_err(global_allocator.*) catch |err| {
        std.log.err("Failed to init window {}\n", .{err});
        @panic("Failed to init window");
    };
}

fn loadGL() void {
    gl.load(null, struct {
        fn getProcAddress(ctx: @TypeOf(null), proc: [:0]const u8) ?gl.FunctionPointer {
            _ = ctx;
            return @ptrCast(c.SDL_GL_GetProcAddress(proc));
        }
    }.getProcAddress) catch |err| {
        std.log.debug("Failed to load gl funcs {}\n", .{err});
        @panic("gl.load");
    };
}

fn checkGLError() void {
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

const mesh_program = a.ShaderPrograms.mesh;

export fn game_init(global_allocator: *std.mem.Allocator) void {
    std.log.debug("game_init\n", .{});
    g_mem = global_allocator.create(GameMemory) catch @panic("OOM");
    const frame_arena_buffer = global_allocator.alloc(u8, FRAME_ARENA_SIZE) catch @panic("OOM");
    g_mem.* = .{
        .global_allocator = global_allocator.*,
        .frame_fba = std.heap.FixedBufferAllocator.init(frame_arena_buffer),
        .assetman = AssetManager.init(global_allocator.*, g_mem.frame_fba.allocator()),
    };
    std.log.debug("actual ptr: {}, correct ptr {}", .{ g_mem.assetman.frame_arena.ptr, g_mem.frame_fba.allocator().ptr });
    g_assetman = &g_mem.assetman;
    g_mem.performance_frequency = c.SDL_GetPerformanceFrequency();
    g_mem.last_frame_time = c.SDL_GetPerformanceCounter();

    loadGL();

    var majorVer: gl.GLint = 0;
    var minorVer: gl.GLint = 0;
    gl.getIntegerv(gl.MAJOR_VERSION, &majorVer);
    gl.getIntegerv(gl.MINOR_VERSION, &minorVer);
    std.log.debug("OpenGL Version: {}.{}", .{ majorVer, minorVer });

    gl.viewport(0, 0, g_init.width, g_init.height);

    // MESH PROGRAM
    // const mesh_program_name = g_assetman.resolveShaderProgram(mesh_program).program;

    // !NOTE: IMPORTANT: never do this again, it messes up ubo block bindings

    // gl.uniformBlockBinding(mesh_program_name, 0, UBO.CameraMatrices.value());
    // gl.uniformBlockBinding(mesh_program_name, 1, UBO.PointLights.value());

    // MESH VAO
    var vao: gl.GLuint = 0;
    gl.createVertexArrays(1, &vao);
    std.debug.assert(vao != 0);
    g_mem.mesh_vao = vao;

    // positions
    // gl.vertexArrayVertexBuffer(vao, 0, vertices, 0, @sizeOf(formats.Vector3));
    gl.enableVertexArrayAttrib(vao, Attrib.Position.value());
    gl.vertexArrayAttribBinding(vao, Attrib.Position.value(), 0);
    gl.vertexArrayAttribFormat(vao, Attrib.Position.value(), 3, gl.FLOAT, gl.FALSE, 0);

    // normals
    // gl.vertexArrayVertexBuffer(vao, 1, normals, 0, @sizeOf(formats.Vector3));
    gl.vertexArrayAttribBinding(vao, Attrib.Normal.value(), 1);
    gl.vertexArrayAttribFormat(vao, Attrib.Normal.value(), 3, gl.FLOAT, gl.FALSE, 0);
    gl.enableVertexArrayAttrib(vao, Attrib.Normal.value());

    // uvs
    // gl.vertexArrayVertexBuffer(vao, 1, normals, 0, @sizeOf(formats.Vector3));
    gl.vertexArrayAttribBinding(vao, Attrib.UV.value(), 1);
    gl.vertexArrayAttribFormat(vao, Attrib.UV.value(), 2, gl.FLOAT, gl.FALSE, 0);
    gl.enableVertexArrayAttrib(vao, Attrib.UV.value());

    const PERSISTENT_BUFFER_FLAGS: gl.GLbitfield = gl.MAP_PERSISTENT_BIT | gl.MAP_WRITE_BIT | gl.MAP_COHERENT_BIT;

    // Camera matrices ubo
    {
        var camera_ubo: gl.GLuint = 0;
        gl.createBuffers(1, &camera_ubo);

        gl.namedBufferStorage(
            camera_ubo,
            @sizeOf(CameraMatrices) * MAX_FRAMES_QUEUED,
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const camera_matrices_c: [*c]CameraMatrices = @alignCast(@ptrCast(gl.mapNamedBufferRange(camera_ubo, 0, @sizeOf(CameraMatrices) * MAX_FRAMES_QUEUED, PERSISTENT_BUFFER_FLAGS) orelse {
            checkGLError();
            @panic("bind camera_ubo");
        }));
        const camera_matrices = camera_matrices_c[0..MAX_FRAMES_QUEUED];
        g_mem.camera_ubo = camera_ubo;
        g_mem.camera_matrices = camera_matrices;
    }

    // Point lights ubo
    {
        var point_lights_ubo: gl.GLuint = 0;
        gl.createBuffers(1, &point_lights_ubo);

        gl.namedBufferStorage(
            point_lights_ubo,
            @sizeOf(RenderPointLightArray) * MAX_FRAMES_QUEUED,
            null,
            PERSISTENT_BUFFER_FLAGS,
        );
        const point_lights_c: [*c]RenderPointLightArray = @alignCast(@ptrCast(gl.mapNamedBufferRange(
            point_lights_ubo,
            0,
            @sizeOf(RenderPointLightArray) * MAX_FRAMES_QUEUED,
            PERSISTENT_BUFFER_FLAGS,
        ) orelse {
            checkGLError();
            @panic("bind point_lights_ubo");
        }));
        const point_lights = point_lights_c[0..MAX_FRAMES_QUEUED];
        g_mem.point_lights_ubo = point_lights_ubo;
        g_mem.point_lights = point_lights;
    }

    _ = g_mem.world.addEntity(.{
        .transform = .{ .pos = Vec3.new(1, 1, 0) },
        .flags = .{ .point_light = true, .rotate = true },
        .point_light = .{ .color_intensity = Vec4.new(1.0, 0.3, 0.1, 100.0), .radius = 0.1 },
        .rotate = .{ .axis = Vec3.up(), .rate = 60 },
    });

    _ = g_mem.world.addEntity(.{
        .transform = .{ .pos = Vec3.new(-1, 1, 0) },
        .flags = .{ .point_light = true, .rotate = true },
        .point_light = .{
            .color_intensity = Vec4.new(0.2, 0.5, 1.0, 100.0),
            .radius = 0.1,
        },
        .rotate = .{ .axis = Vec3.up(), .rate = -20 },
    });

    // Plane
    _ = g_mem.world.addEntity(.{
        .flags = .{ .mesh = true },
        .transform = .{ .scale = Vec3.one().scale(100) },
        .mesh = .{ .handle = a.Meshes.plane },
    });

    // 10 bunnies
    {
        for (0..10) |i| {
            _ = g_mem.world.addEntity(.{
                .transform = .{ .pos = Vec3.new(@as(f32, @floatFromInt(i)) * 0.3, -0.03, 0) },

                .flags = .{ .mesh = true },
                .mesh = .{ .handle = a.Meshes.bunny },
            });
        }
    }
}

// TODO: move this out into a renderer
// Should be std140
pub const CameraMatrices = extern struct {
    projection: Mat4,
    view: Mat4,
};
pub const RenderPointLight = extern struct {
    pos_radius: Vec4, // x, y, z - pos, w - radius
    color_intensity: Vec4, // x, y, z - color, w - intensity
};

pub const RenderPointLightArray = extern struct {
    lights: [MAX_POINT_LIGHTS]RenderPointLight,
    count: c_uint,
};

export fn game_update() bool {
    const ginit = g_init;
    const gmem = g_mem;
    // std.debug.print("FPS: {d}\n", .{1.0 / g_mem.delta_time});

    gmem.frame_fba.reset();
    var event: c.SDL_Event = undefined;

    var move = Vec3.zero();
    var look = Vec2.zero();

    while (c.SDL_PollEvent(&event) != 0) {
        switch (event.type) {
            c.SDL_QUIT => {
                return false;
            },
            c.SDL_MOUSEMOTION => {
                if (gmem.mouse_focus) {
                    look.xMut().* += @floatFromInt(event.motion.xrel);
                    look.yMut().* += @floatFromInt(event.motion.yrel);
                }
            },
            c.SDL_MOUSEBUTTONUP => {
                if (!gmem.mouse_focus) {
                    _ = c.SDL_SetRelativeMouseMode(c.SDL_TRUE);

                    gmem.mouse_focus = true;
                }
            },
            c.SDL_MOUSEWHEEL => {
                if (gmem.mouse_focus) {
                    gmem.free_cam.move_speed = @max(gmem.free_cam.move_speed + event.wheel.preciseY * 0.1, 0);
                }
            },
            c.SDL_KEYUP, c.SDL_KEYDOWN => {
                const pressed = event.key.state == c.SDL_PRESSED;
                switch (event.key.keysym.scancode) {
                    c.SDL_SCANCODE_ESCAPE => {
                        if (event.type == c.SDL_KEYUP) {
                            if (gmem.mouse_focus) {
                                _ = c.SDL_SetRelativeMouseMode(c.SDL_FALSE);
                                gmem.mouse_focus = false;
                            } else {
                                return false;
                            }
                        }
                    },
                    c.SDL_SCANCODE_W => {
                        gmem.input_state.forward = pressed;
                    },
                    c.SDL_SCANCODE_S => {
                        gmem.input_state.backward = pressed;
                    },
                    c.SDL_SCANCODE_A => {
                        gmem.input_state.left = pressed;
                    },
                    c.SDL_SCANCODE_D => {
                        gmem.input_state.right = pressed;
                    },
                    c.SDL_SCANCODE_SPACE => {
                        gmem.input_state.up = pressed;
                    },
                    c.SDL_SCANCODE_LCTRL => {
                        gmem.input_state.down = pressed;
                    },
                    else => {},
                }
            },
            c.SDL_WINDOWEVENT => {
                switch (event.window.event) {
                    c.SDL_WINDOWEVENT_SIZE_CHANGED => {
                        ginit.width = event.window.data1;
                        ginit.height = event.window.data2;
                        std.log.debug("w: {}, h: {}\n", .{ ginit.width, ginit.height });

                        gl.viewport(0, 0, ginit.width, ginit.height);
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    const now = c.SDL_GetPerformanceCounter();
    gmem.delta_time = @as(f32, @floatFromInt((now - gmem.last_frame_time))) / @as(f32, @floatFromInt(gmem.performance_frequency));
    gmem.last_frame_time = now;

    if (gmem.input_state.forward) {
        //const y = &move.data[1];
        move.yMut().* += 1;
    }
    if (gmem.input_state.backward) {
        move.yMut().* -= 1;
    }
    if (gmem.input_state.left) {
        move.xMut().* -= 1;
    }
    if (gmem.input_state.right) {
        move.xMut().* += 1;
    }
    if (gmem.input_state.up) {
        move.zMut().* += 1;
    }
    if (gmem.input_state.down) {
        move.zMut().* -= 1;
    }

    // TODO: make this an entity
    gmem.free_cam.update(gmem.delta_time, move, look.scale(0.008));

    // RENDER
    // gl.fenceSync(_condition: GLenum, _flags: GLbitfield)

    const f_width: f32 = @floatFromInt(ginit.width);
    const f_height: f32 = @floatFromInt(ginit.height);
    gmem.tripple_buffer_index = (gmem.tripple_buffer_index + 1) % MAX_FRAMES_QUEUED;

    gl.enable(gl.CULL_FACE);
    gl.enable(gl.DEPTH_TEST);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(g_assetman.resolveShaderProgram(a.ShaderPrograms.mesh).program);
    gl.bindVertexArray(gmem.mesh_vao);

    if (gmem.gl_fences[gmem.tripple_buffer_index]) |fence| {
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
        gmem.gl_fences[gmem.tripple_buffer_index] = null;
    }

    gmem.gl_fences[gmem.tripple_buffer_index] = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);

    {
        const camera_matrix = &gmem.camera_matrices[gmem.tripple_buffer_index];

        camera_matrix.* = .{
            .projection = Mat4.perspective(
                60,
                f_width / f_height,
                0.1,
                100.0,
            ),
            .view = gmem.free_cam.view_matrix,
        };

        gl.bindBufferRange(
            gl.UNIFORM_BUFFER,
            UBO.CameraMatrices.value(),
            gmem.camera_ubo,
            gmem.tripple_buffer_index * @sizeOf(CameraMatrices),
            @sizeOf(CameraMatrices),
        );
    }

    // Collect point lights
    {
        const point_lights = &gmem.point_lights[gmem.tripple_buffer_index];
        point_lights.count = 0;

        for (0..gmem.world.entity_count) |i| {
            const ent = &gmem.world.entities[i];
            if (!ent.flags.active) continue;

            if (ent.flags.rotate) {
                const old_pos = ent.transform.pos;
                const new_pos = Mat4.fromRotation(
                    ent.rotate.rate * gmem.delta_time,
                    ent.rotate.axis,
                ).mulByVec4(Vec4.new(old_pos.x(), old_pos.y(), old_pos.z(), 1));
                ent.transform.pos = Vec3.new(new_pos.x(), new_pos.y(), new_pos.z());
            }

            if (ent.flags.point_light) {
                const pos = ent.transform.pos;
                point_lights.lights[point_lights.count] = .{
                    .pos_radius = Vec4.new(pos.x(), pos.y(), pos.z(), ent.point_light.radius),
                    .color_intensity = ent.point_light.color_intensity,
                };
                point_lights.count += 1;
                if (point_lights.count == MAX_POINT_LIGHTS) {
                    break;
                }
            }
        }

        // gl.flushMappedNamedBufferRange(
        //     gmem.point_lights_ubo,
        //     gmem.tripple_buffer_index * @sizeOf(RenderPointLightArray),
        //     @sizeOf(RenderPointLightArray),
        // );

        gl.bindBufferRange(
            gl.UNIFORM_BUFFER,
            UBO.PointLights.value(),
            gmem.point_lights_ubo,
            gmem.tripple_buffer_index * @sizeOf(RenderPointLightArray),
            @sizeOf(RenderPointLightArray),
        );
    }

    gmem.rotation += 60 * gmem.delta_time;

    // Render meshes and lights
    for (0..gmem.world.entity_count) |i| {
        const ent = &gmem.world.entities[i];
        if (!ent.flags.active) continue;

        if (ent.flags.mesh or ent.flags.point_light) {
            const color = if (ent.flags.mesh) ent.mesh.color else ent.point_light.color();
            gl.uniformMatrix4fv(1, 1, gl.FALSE, @ptrCast(&ent.transform.matrix().data));
            gl.uniform3fv(2, 1, @ptrCast(&color.data));

            const mesh_handle = if (ent.flags.mesh) ent.mesh.handle else a.Meshes.sphere;
            const mesh = g_assetman.resolveMesh(mesh_handle);
            mesh.positions.bind(Attrib.Position.value());
            mesh.normals.bind(Attrib.Normal.value());
            mesh.uvs.bind(Attrib.UV.value());
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);
            gl.drawElements(
                gl.TRIANGLES,
                mesh.indices.count,
                mesh.indices.type,
                @ptrFromInt(mesh.indices.offset),
            );
        }
    }

    c.SDL_GL_SwapWindow(ginit.window);
    // const vblank_event: D3DKMT_WAITFORVERTICALBLANKEVENT = .{
    //     .hAdapter = 0,
    //     .hDevice = ginit.syswm_info.info.win.hdc.*,
    //     .VidPnSourceId = 0,
    // };
    // switch (D3DKMTWaitForVerticalBlankEvent(&vblank_event)) {
    //     .SUCCESS => {},
    //     else => |status| {
    //         std.log.err("Failed to wait for vblank {}\n", .{status});
    //         return false;
    //     },
    // }

    // c.SDL_Delay(14);

    g_assetman.watchChanges();

    return true;
}

export fn game_shutdown() void {
    const gmem = g_mem;
    std.log.debug("game_shutdown\n", .{});
    gmem.global_allocator.free(gmem.frame_fba.buffer);
    gmem.global_allocator.destroy(gmem);
}

export fn game_shutdown_window() void {
    std.log.debug("game_shutdown_window\n", .{});
    c.SDL_GL_DeleteContext(g_init.context);
    c.SDL_DestroyWindow(g_init.window);
    g_init.global_allocator.destroy(g_init);
    g_init_exists = false;
    c.SDL_Quit();
}

export fn game_hot_reload(init_memory: ?*anyopaque, gmemory: ?*anyopaque) void {
    std.log.debug("game_hot_reload {any} {any}\n", .{ init_memory, gmemory });
    if (init_memory) |init_mem| {
        g_init = @alignCast(@ptrCast(init_mem));
        g_init_exists = true;
        loadGL();
    }
    if (gmemory) |gmem| {
        g_mem = @alignCast(@ptrCast(gmem));
        g_assetman = &g_mem.assetman;
    }
    if (g_init_exists) {
        c.SDL_RaiseWindow(g_init.window);
    }
}

export fn game_memory() *anyopaque {
    return @ptrCast(g_mem);
}

export fn game_init_memory() *anyopaque {
    return @ptrCast(g_init);
}

export fn game_memory_size() usize {
    return @sizeOf(GameMemory);
}

export fn game_init_memory_size() usize {
    return @sizeOf(InitMemory);
}

fn sdl_try(result: c_int) !void {
    if (result < 0) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLError;
    }
}
