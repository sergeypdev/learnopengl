const std = @import("std");
const c = @import("sdl.zig");
const manymouse = @import("manymouse.zig");
const gl = @import("gl.zig");
const AssetManager = @import("AssetManager.zig");
const formats = @import("formats.zig");
const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Mat4 = za.Mat4;
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

const DEFAULT_WIDTH = 800;
const DEFAULT_HEIGHT = 600;

// TODO: move out into renderer file maybe
const Attrib = enum(gl.GLuint) {
    Position = 0,
    Normal = 1,

    pub inline fn value(self: Attrib) gl.GLuint {
        return @intFromEnum(self);
    }
};
const UBO = enum(gl.GLuint) {
    CameraMatrices = 0,

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

pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    frame_fba: std.heap.FixedBufferAllocator,
    assetman: AssetManager,
    performance_frequency: u64 = 0,
    last_frame_time: u64 = 0,
    delta_time: f32 = 0.0000001,
    mesh_vao: gl.GLuint = 0,
    camera_ubo: gl.GLuint = 0,
    camera_matrices: []CameraMatrices = &.{},
    current_camera_matrix: usize = 0,
    rotation: f32 = 0,
    input_state: InputState = .{},
    free_cam: FreeLookCamera = .{},
    mouse_focus: bool = false,
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
    view_matrix: Mat4 = Mat4.identity(),

    pub fn update(self: *FreeLookCamera, move: Vec3, look: Vec2) void {
        self.yaw += look.x();
        self.pitch += look.y();
        // First rotate pitch, then yaw
        const rot = Mat4.fromRotation(self.pitch, Vec3.left()).mul(Mat4.fromRotation(self.yaw, Vec3.up()));

        // First 3 of transform matrix are: right, up, forward
        const left = Vec3.new(rot.data[0][0], rot.data[1][0], rot.data[2][0]);
        const up = Vec3.new(rot.data[0][1], rot.data[1][1], rot.data[2][1]);
        const forward = Vec3.new(rot.data[0][2], rot.data[1][2], rot.data[2][2]);

        const movement = left.scale(-move.x()).add(forward.scale(move.y())).add(up.scale(move.z()));

        self.pos = self.pos.add(movement);

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
        c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_ALLOW_HIGHDPI,
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
    const mesh_program_name = g_assetman.resolveShaderProgram(mesh_program).program;

    gl.uniformBlockBinding(mesh_program_name, 0, UBO.CameraMatrices.value());

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

    var camera_ubo: gl.GLuint = 0;
    gl.createBuffers(1, &camera_ubo);

    const CAMERA_MATRICES_COUNT = 120;
    const PERSISTENT_BUFFER_FLAGS: gl.GLbitfield = gl.MAP_PERSISTENT_BIT | gl.MAP_WRITE_BIT | gl.MAP_COHERENT_BIT;
    gl.namedBufferStorage(
        camera_ubo,
        @sizeOf(CameraMatrices) * CAMERA_MATRICES_COUNT,
        null,
        PERSISTENT_BUFFER_FLAGS,
    );
    const camera_matrices_c: [*c]CameraMatrices = @alignCast(@ptrCast(gl.mapNamedBufferRange(camera_ubo, 0, @sizeOf(CameraMatrices) * CAMERA_MATRICES_COUNT, PERSISTENT_BUFFER_FLAGS) orelse {
        checkGLError();
        @panic("bind camera_ubo");
    }));
    const camera_matrices = camera_matrices_c[0..CAMERA_MATRICES_COUNT];
    g_mem.camera_ubo = camera_ubo;
    g_mem.camera_matrices = camera_matrices;
}

// Should be std140
const CameraMatrices = extern struct {
    projection: za.Mat4,
    view: za.Mat4,
};

export fn game_update() bool {
    // std.debug.print("FPS: {d}\n", .{1.0 / g_mem.delta_time});

    g_mem.frame_fba.reset();
    var event: c.SDL_Event = undefined;

    var move = Vec3.zero();
    var look = Vec2.zero();

    while (c.SDL_PollEvent(&event) != 0) {
        switch (event.type) {
            c.SDL_QUIT => {
                return false;
            },
            c.SDL_MOUSEMOTION => {
                if (g_mem.mouse_focus) {
                    look.xMut().* += @floatFromInt(event.motion.xrel);
                    look.yMut().* += @floatFromInt(event.motion.yrel);
                }
            },
            c.SDL_MOUSEBUTTONUP => {
                if (!g_mem.mouse_focus) {
                    _ = c.SDL_SetRelativeMouseMode(c.SDL_TRUE);

                    g_mem.mouse_focus = true;
                }
            },
            c.SDL_KEYUP, c.SDL_KEYDOWN => {
                const pressed = event.key.state == c.SDL_PRESSED;
                switch (event.key.keysym.scancode) {
                    c.SDL_SCANCODE_ESCAPE => {
                        if (event.type == c.SDL_KEYUP) {
                            if (g_mem.mouse_focus) {
                                _ = c.SDL_SetRelativeMouseMode(c.SDL_FALSE);
                                g_mem.mouse_focus = false;
                            } else {
                                return false;
                            }
                        }
                    },
                    c.SDL_SCANCODE_W => {
                        g_mem.input_state.forward = pressed;
                    },
                    c.SDL_SCANCODE_S => {
                        g_mem.input_state.backward = pressed;
                    },
                    c.SDL_SCANCODE_A => {
                        g_mem.input_state.left = pressed;
                    },
                    c.SDL_SCANCODE_D => {
                        g_mem.input_state.right = pressed;
                    },
                    c.SDL_SCANCODE_SPACE => {
                        g_mem.input_state.up = pressed;
                    },
                    c.SDL_SCANCODE_LSHIFT => {
                        g_mem.input_state.down = pressed;
                    },
                    else => {},
                }
            },
            c.SDL_WINDOWEVENT => {
                switch (event.window.event) {
                    c.SDL_WINDOWEVENT_SIZE_CHANGED => {
                        g_init.width = event.window.data1;
                        g_init.height = event.window.data2;
                        std.log.debug("w: {}, h: {}\n", .{ g_init.width, g_init.height });

                        gl.viewport(0, 0, g_init.width, g_init.height);
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    const now = c.SDL_GetPerformanceCounter();
    g_mem.delta_time = @as(f32, @floatFromInt((now - g_mem.last_frame_time))) / @as(f32, @floatFromInt(g_mem.performance_frequency));
    g_mem.last_frame_time = now;

    const MOVEMENT_SPEED = 0.5;
    if (g_mem.input_state.forward) {
        //const y = &move.data[1];
        move.yMut().* += 1;
    }
    if (g_mem.input_state.backward) {
        move.yMut().* -= 1;
    }
    if (g_mem.input_state.left) {
        move.xMut().* -= 1;
    }
    if (g_mem.input_state.right) {
        move.xMut().* += 1;
    }
    if (g_mem.input_state.up) {
        move.zMut().* += 1;
    }
    if (g_mem.input_state.down) {
        move.zMut().* -= 1;
    }

    move = move.scale(MOVEMENT_SPEED * g_mem.delta_time);

    g_mem.free_cam.update(move, look.scale(0.008));

    // RENDER
    // gl.fenceSync(_condition: GLenum, _flags: GLbitfield)

    const f_width: f32 = @floatFromInt(g_init.width);
    const f_height: f32 = @floatFromInt(g_init.height);
    g_mem.current_camera_matrix = (g_mem.current_camera_matrix + 1) % g_mem.camera_matrices.len;
    const camera_matrix = &g_mem.camera_matrices[g_mem.current_camera_matrix];

    camera_matrix.* = .{
        .projection = Mat4.perspective(
            30,
            f_width / f_height,
            0.1,
            100.0,
        ),
        .view = g_mem.free_cam.view_matrix,
    };
    gl.enable(gl.CULL_FACE);
    gl.enable(gl.DEPTH_TEST);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(g_assetman.resolveShaderProgram(a.ShaderPrograms.mesh).program);
    gl.bindVertexArray(g_mem.mesh_vao);

    gl.bindBufferRange(
        gl.UNIFORM_BUFFER,
        UBO.CameraMatrices.value(),
        g_mem.camera_ubo,
        g_mem.current_camera_matrix * @sizeOf(CameraMatrices),
        @sizeOf(CameraMatrices),
    );
    g_mem.rotation += 0.5 * g_mem.delta_time;
    gl.uniformMatrix4fv(1, 1, gl.FALSE, @ptrCast(&Mat4.fromRotation(g_mem.rotation, Vec3.up()).data));

    const mesh = g_assetman.resolveMesh(a.Meshes.bunny);
    mesh.positions.bind(Attrib.Position.value());
    mesh.normals.bind(Attrib.Normal.value());
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indices.buffer);
    gl.drawElements(
        gl.TRIANGLES,
        mesh.indices.count,
        mesh.indices.type,
        @ptrFromInt(mesh.indices.offset),
    );

    c.SDL_GL_SwapWindow(g_init.window);
    DwmFlush();
    // const vblank_event: D3DKMT_WAITFORVERTICALBLANKEVENT = .{
    //     .hAdapter = 0,
    //     .hDevice = g_init.syswm_info.info.win.hdc.*,
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
    std.log.debug("game_shutdown\n", .{});
    g_mem.global_allocator.free(g_mem.frame_fba.buffer);
    g_mem.global_allocator.destroy(g_mem);
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
