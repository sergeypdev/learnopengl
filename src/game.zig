const std = @import("std");
const globals = @import("globals.zig");
const InitMemory = globals.InitMemory;
const GameMemory = globals.GameMemory;
const c = @import("sdl.zig");
const gl = @import("gl.zig");
const AssetManager = @import("AssetManager.zig");
const Render = @import("Render.zig");
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

fn game_init_window_err(global_allocator: std.mem.Allocator) !void {
    if (c.SDL_SetHint(c.SDL_HINT_WINDOWS_DPI_AWARENESS_, "permonitorv2") == c.SDL_FALSE) {
        std.log.debug("Failed to setup windows DPI scaling\n", .{});
    }
    if (c.SDL_SetHint(c.SDL_HINT_WINDOWS_DPI_SCALING_, "1") == c.SDL_FALSE) {
        std.log.debug("Failed to setup windows DPI scaling\n", .{});
    }
    // _ = DwmEnableMMCSS(1);
    try sdl_try(c.SDL_Init(c.SDL_INIT_EVERYTHING));

    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_DOUBLEBUFFER, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, 4));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, 5));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, c.SDL_GL_CONTEXT_PROFILE_CORE));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_FRAMEBUFFER_SRGB_CAPABLE, 0));

    const maybe_window = c.SDL_CreateWindow(
        "Learn OpenGL with Zig!",
        c.SDL_WINDOWPOS_UNDEFINED,
        c.SDL_WINDOWPOS_UNDEFINED,
        globals.DEFAULT_WIDTH,
        globals.DEFAULT_HEIGHT,
        c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_ALLOW_HIGHDPI | c.SDL_WINDOW_RESIZABLE,
    );
    if (maybe_window == null) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLWindowError;
    }
    const window = maybe_window.?;

    const context = c.SDL_GL_CreateContext(window);

    try sdl_try(c.SDL_GL_SetSwapInterval(0));

    globals.g_init = try global_allocator.create(InitMemory);
    globals.g_init_exists = true;
    globals.g_init.* = .{
        .global_allocator = global_allocator,
        .window = window,
        .context = context,
        .width = globals.DEFAULT_WIDTH,
        .height = globals.DEFAULT_HEIGHT,
    };

    const version = &globals.g_init.syswm_info.version;
    version.major = c.SDL_MAJOR_VERSION;
    version.minor = c.SDL_MINOR_VERSION;
    version.patch = c.SDL_PATCHLEVEL;

    c.SDL_GL_GetDrawableSize(window, &globals.g_init.width, &globals.g_init.height);

    if (c.SDL_GetWindowWMInfo(window, &globals.g_init.syswm_info) == 0) {
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
    const getProcAddress = struct {
        fn getProcAddress(ctx: @TypeOf(null), proc: [:0]const u8) ?gl.FunctionPointer {
            _ = ctx;
            return @ptrCast(c.SDL_GL_GetProcAddress(proc));
        }
    }.getProcAddress;
    gl.load(null, getProcAddress) catch |err| {
        std.log.debug("Failed to load gl funcs {}\n", .{err});
        @panic("gl.load");
    };
    gl.GL_ARB_bindless_texture.load(null, getProcAddress) catch |err| {
        std.log.debug("Failed to load gl funcs GL_ARB_bindless_texture {}\n", .{err});
        @panic("gl.load");
    };
}

const mesh_program = a.ShaderPrograms.mesh;

export fn game_init(global_allocator: *std.mem.Allocator) void {
    loadGL();

    std.log.debug("game_init\n", .{});
    globals.g_mem = global_allocator.create(GameMemory) catch @panic("OOM");
    const frame_arena_buffer = global_allocator.alloc(u8, FRAME_ARENA_SIZE) catch @panic("OOM");
    globals.g_mem.* = .{
        .global_allocator = global_allocator.*,
        .frame_fba = std.heap.FixedBufferAllocator.init(frame_arena_buffer),
        .assetman = AssetManager.init(global_allocator.*, globals.g_mem.frame_fba.allocator()),
        .render = Render.init(global_allocator.*, globals.g_mem.frame_fba.allocator(), &globals.g_mem.assetman),
    };
    globals.g_mem.render.camera = &globals.g_mem.free_cam.camera;
    std.log.debug("actual ptr: {}, correct ptr {}", .{ globals.g_mem.assetman.frame_arena.ptr, globals.g_mem.frame_fba.allocator().ptr });
    globals.g_assetman = &globals.g_mem.assetman;
    globals.g_mem.performance_frequency = c.SDL_GetPerformanceFrequency();
    globals.g_mem.last_frame_time = c.SDL_GetPerformanceCounter();

    var majorVer: gl.GLint = 0;
    var minorVer: gl.GLint = 0;
    gl.getIntegerv(gl.MAJOR_VERSION, &majorVer);
    gl.getIntegerv(gl.MINOR_VERSION, &minorVer);
    std.log.debug("OpenGL Version: {}.{}", .{ majorVer, minorVer });

    gl.viewport(0, 0, globals.g_init.width, globals.g_init.height);

    _ = globals.g_mem.world.addEntity(.{
        .transform = .{ .pos = Vec3.new(1, 1, 0) },
        .flags = .{ .point_light = true, .rotate = true },
        .point_light = .{ .color_intensity = Vec4.new(1.0, 0.3, 0.1, 100.0), .radius = 0.1 },
        .rotate = .{ .axis = Vec3.up(), .rate = 60 },
    });

    _ = globals.g_mem.world.addEntity(.{
        .transform = .{ .pos = Vec3.new(-1, 1, 0) },
        .flags = .{ .point_light = true, .rotate = true },
        .point_light = .{
            .color_intensity = Vec4.new(0.2, 0.5, 1.0, 100.0),
            .radius = 0.1,
        },
        .rotate = .{ .axis = Vec3.up(), .rate = -20 },
    });

    // Plane
    _ = globals.g_mem.world.addEntity(.{
        .flags = .{ .mesh = true },
        .transform = .{ .scale = Vec3.one().scale(1) },
        .mesh = .{
            .handle = a.Meshes.plane,
            .material = .{
                .normal_map = a.Textures.@"tile.norm",
            },
        },
    });

    // 10 bunnies
    {
        for (0..10) |i| {
            _ = globals.g_mem.world.addEntity(.{
                .transform = .{ .pos = Vec3.new(@as(f32, @floatFromInt(i)) * 0.3, 0, 0) },

                .flags = .{ .mesh = true },
                .mesh = .{
                    .handle = a.Meshes.bunny,
                    .material = .{
                        .albedo_map = a.Textures.bunny_tex1,
                        // .normal_map = a.Textures.@"tile.norm",
                    },
                },
            });
        }
    }
}

export fn game_update() bool {
    const ginit = globals.g_init;
    const gmem = globals.g_mem;
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
                    // Toggle fullscreen
                    c.SDL_SCANCODE_F11 => {
                        if (event.type == c.SDL_KEYDOWN) {
                            toggleFullScreen() catch continue;
                        }
                    },
                    // Toggle vsync
                    c.SDL_SCANCODE_F10 => {
                        if (event.type == c.SDL_KEYDOWN) {
                            const newSwap: c_int = if (ginit.vsync) 0 else 1;
                            sdl_try(c.SDL_GL_SetSwapInterval(newSwap)) catch continue;
                            ginit.vsync = !ginit.vsync;
                        }
                    },
                    c.SDL_SCANCODE_ESCAPE => {
                        if (event.type == c.SDL_KEYUP) {
                            if (ginit.fullscreen) {
                                toggleFullScreen() catch continue;
                            } else if (gmem.mouse_focus) {
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
                        c.SDL_GL_GetDrawableSize(ginit.window, &ginit.width, &ginit.height);
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

    const f_width: f32 = @floatFromInt(ginit.width);
    const f_height: f32 = @floatFromInt(ginit.height);

    gmem.free_cam.camera.aspect = f_width / f_height;
    gmem.rotation += 60 * gmem.delta_time;

    {
        gmem.render.begin();
        defer gmem.render.finish();

        // Collect point lights
        {
            const point_lights = gmem.render.getPointLights();
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
                    if (point_lights.count == Render.MAX_POINT_LIGHTS) {
                        break;
                    }
                }
            }
        }

        // Render meshes and lights
        for (0..gmem.world.entity_count) |i| {
            const ent = &gmem.world.entities[i];
            if (!ent.flags.active) continue;

            if (ent.flags.mesh) {
                gmem.render.draw(.{
                    .mesh = ent.mesh.handle,
                    .material = ent.mesh.material,
                    .transform = ent.transform.matrix(),
                });
            } else if (ent.flags.point_light) {
                gmem.render.draw(.{
                    .mesh = a.Meshes.sphere,
                    .material = .{ .albedo = ent.point_light.color() },
                    .transform = ent.transform.matrix(),
                });
            }
        }
    }

    globals.g_assetman.watchChanges();

    return true;
}

export fn game_shutdown() void {
    const gmem = globals.g_mem;
    std.log.debug("game_shutdown\n", .{});
    gmem.global_allocator.free(gmem.frame_fba.buffer);
    gmem.global_allocator.destroy(gmem);
}

export fn game_shutdown_window() void {
    std.log.debug("game_shutdown_window\n", .{});
    c.SDL_GL_DeleteContext(globals.g_init.context);
    c.SDL_DestroyWindow(globals.g_init.window);
    globals.g_init.global_allocator.destroy(globals.g_init);
    globals.g_init_exists = false;
    c.SDL_Quit();
}

export fn game_hot_reload(init_memory: ?*anyopaque, gmemory: ?*anyopaque) void {
    std.log.debug("game_hot_reload {any} {any}\n", .{ init_memory, gmemory });
    if (init_memory) |init_mem| {
        globals.g_init = @alignCast(@ptrCast(init_mem));
        globals.g_init_exists = true;
        loadGL();
    }
    if (gmemory) |gmem| {
        globals.g_mem = @alignCast(@ptrCast(gmem));
        globals.g_assetman = &globals.g_mem.assetman;
    }
    if (globals.g_init_exists) {
        c.SDL_RaiseWindow(globals.g_init.window);
    }
}

export fn game_memory() *anyopaque {
    return @ptrCast(globals.g_mem);
}

export fn game_init_memory() *anyopaque {
    return @ptrCast(globals.g_init);
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

fn toggleFullScreen() !void {
    const ginit = globals.g_init;
    const current_flags: c.Uint32 = if (globals.g_init.fullscreen) c.SDL_WINDOW_FULLSCREEN else 0;
    const new_flags: c.Uint32 = if (globals.g_init.fullscreen) 0 else c.SDL_WINDOW_FULLSCREEN;

    const display_index = c.SDL_GetWindowDisplayIndex(ginit.window);
    var mode: c.SDL_DisplayMode = undefined;
    try sdl_try(c.SDL_GetDesktopDisplayMode(display_index, &mode));

    // When going fullscreen set a good display mode
    if (!ginit.fullscreen) {
        try sdl_try(c.SDL_SetWindowDisplayMode(ginit.window, &mode));
    }

    try sdl_try(c.SDL_SetWindowFullscreen(ginit.window, new_flags));
    errdefer {
        sdl_try(c.SDL_SetWindowFullscreen(ginit.window, current_flags)) catch {
            std.log.err("Failed to change fullscreen mode and then failed to restore the old fullscreen mode\n", .{});
        };
    }

    ginit.fullscreen = !ginit.fullscreen;
}
