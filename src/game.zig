const std = @import("std");
const c = @import("c.zig");

pub const InitMemory = struct {
    global_allocator: std.mem.Allocator,
    window: *c.SDL_Window,
    context: ?*anyopaque,
    some_var: i32 = 0,
};

pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    counter: i32 = 0,
};

var g_init: *InitMemory = undefined;
var g_mem: *GameMemory = undefined;

fn game_init_window_err(global_allocator: std.mem.Allocator) !void {
    try sdl_try(c.SDL_Init(c.SDL_INIT_EVERYTHING));

    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_DOUBLEBUFFER, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, 4));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, c.SDL_GL_CONTEXT_PROFILE_CORE));

    const maybe_window = c.SDL_CreateWindow("Learn OpenGL with Zig!", c.SDL_WINDOWPOS_CENTERED, c.SDL_WINDOWPOS_CENTERED, 800, 600, c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_OPENGL);
    if (maybe_window == null) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLWindowError;
    }
    const window = maybe_window.?;

    const context = c.SDL_GL_CreateContext(window);

    if (c.gladLoadGLLoader(c.SDL_GL_GetProcAddress) == 0) {
        return error.GladInitError;
    }

    std.log.debug("OpenGL Version: {}.{}", .{ c.GLVersion.major, c.GLVersion.minor });

    g_init = try global_allocator.create(InitMemory);
    g_init.* = .{
        .global_allocator = global_allocator,
        .window = window,
        .context = context,
    };
}

export fn game_init_window(global_allocator: *std.mem.Allocator) void {
    std.log.debug("game_init_window\n", .{});
    game_init_window_err(global_allocator.*) catch |err| {
        std.log.err("Failed to init window {}\n", .{err});
        @panic("Failed to init window");
    };
}

export fn game_init(global_allocator: *std.mem.Allocator) void {
    std.log.debug("game_init\n", .{});
    g_mem = global_allocator.create(GameMemory) catch @panic("OOM");
    g_mem.* = .{
        .global_allocator = global_allocator.*,
    };
}

export fn game_update() bool {
    var event: c.SDL_Event = undefined;

    while (c.SDL_PollEvent(&event) != 0) {
        switch (event.type) {
            c.SDL_QUIT => {
                return false;
            },
            c.SDL_KEYUP => {
                if (event.key.keysym.sym == c.SDLK_ESCAPE) {
                    return false;
                }
            },
            c.SDL_WINDOWEVENT => {
                switch (event.window.type) {
                    c.SDL_WINDOWEVENT_SIZE_CHANGED => {
                        // var width = event.window.data1;
                        // var height = event.window.data2;
                    },
                    else => {},
                }
            },
            else => {},
        }
        g_mem.counter += 1;

        c.glClearColor(0.3, 0.6, 0.3, 1.0);
        c.glClear(c.GL_COLOR_BUFFER_BIT);

        c.SDL_GL_SwapWindow(g_init.window);
        c.SDL_Delay(1);
    }

    return true;
}

export fn game_shutdown() void {
    std.log.debug("game_shutdown\n", .{});
    g_mem.global_allocator.destroy(g_mem);
}

export fn game_shutdown_window() void {
    std.log.debug("game_shutdown_window\n", .{});
    c.SDL_GL_DeleteContext(g_init.context);
    c.SDL_DestroyWindow(g_init.window);
    g_init.global_allocator.destroy(g_init);
    c.SDL_Quit();
}

export fn game_hot_reload(init_memory: ?*anyopaque, gmemory: ?*anyopaque) void {
    std.log.debug("game_hot_reload {any} {any}\n", .{ init_memory, gmemory });
    if (init_memory) |init_mem| {
        g_init = @alignCast(@ptrCast(init_mem));
    }
    if (gmemory) |gmem| {
        g_mem = @alignCast(@ptrCast(gmem));
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
