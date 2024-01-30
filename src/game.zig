const std = @import("std");
const c = @import("c.zig");

const DEFAULT_WIDTH = 800;
const DEFAULT_HEIGHT = 600;

pub const InitMemory = struct {
    global_allocator: std.mem.Allocator,
    window: *c.SDL_Window,
    context: ?*anyopaque,
    width: c_int,
    height: c_int,
};

pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    counter: i32 = 0,
    triangle_vao: c.GLuint = 0,
    triangle_vbo: c.GLuint = 0,
    shader_program: c.GLuint = 0,
};

var g_init_exists = false;
var g_init: *InitMemory = undefined;
var g_mem: *GameMemory = undefined;

fn game_init_window_err(global_allocator: std.mem.Allocator) !void {
    try sdl_try(c.SDL_Init(c.SDL_INIT_EVERYTHING));

    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_DOUBLEBUFFER, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, 4));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, 1));
    try sdl_try(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, c.SDL_GL_CONTEXT_PROFILE_CORE));

    const maybe_window = c.SDL_CreateWindow(
        "Learn OpenGL with Zig!",
        c.SDL_WINDOWPOS_CENTERED,
        c.SDL_WINDOWPOS_CENTERED,
        DEFAULT_WIDTH,
        DEFAULT_HEIGHT,
        c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_RESIZABLE,
    );
    if (maybe_window == null) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLWindowError;
    }
    const window = maybe_window.?;

    const context = c.SDL_GL_CreateContext(window);

    if (c.gladLoadGLLoader(c.SDL_GL_GetProcAddress) == 0) {
        return error.GladInitError;
    }

    c.glViewport(0, 0, DEFAULT_WIDTH, DEFAULT_HEIGHT);

    std.log.debug("OpenGL Version: {}.{}", .{ c.GLVersion.major, c.GLVersion.minor });

    g_init = try global_allocator.create(InitMemory);
    g_init_exists = true;
    g_init.* = .{
        .global_allocator = global_allocator,
        .window = window,
        .context = context,
        .width = DEFAULT_WIDTH,
        .height = DEFAULT_HEIGHT,
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

    c.glGenBuffers(1, &g_mem.triangle_vbo);
    c.glGenVertexArrays(1, &g_mem.triangle_vao);

    c.glBindVertexArray(g_mem.triangle_vao);

    c.glBindBuffer(c.GL_ARRAY_BUFFER, g_mem.triangle_vbo);
    c.glBufferData(c.GL_ARRAY_BUFFER, @sizeOf(@TypeOf(vertices)), &vertices, c.GL_STATIC_DRAW);
    c.glVertexAttribPointer(0, 3, c.GL_FLOAT, c.GL_FALSE, @sizeOf(f32) * 3, @ptrFromInt(0));
    c.glEnableVertexAttribArray(0);

    const vertex_shader = c.glCreateShader(c.GL_VERTEX_SHADER);
    defer c.glDeleteShader(vertex_shader);

    c.glShaderSource(vertex_shader, 1, &[_][*c]const u8{vertex_shader_code}, null);
    c.glCompileShader(vertex_shader);
    var success: c_int = 0;
    c.glGetShaderiv(vertex_shader, c.GL_COMPILE_STATUS, &success);
    if (success == 0) {
        var info_log: [512:0]u8 = undefined;
        c.glGetShaderInfoLog(vertex_shader, info_log.len, null, &info_log);
        std.log.err("ERROR::SHADER::VERTEX::COMPILATION_FAILED\n{s}\n", .{@as([:0]const u8, &info_log)});
    }

    const fragment_shader = c.glCreateShader(c.GL_FRAGMENT_SHADER);
    defer c.glDeleteShader(fragment_shader);

    c.glShaderSource(fragment_shader, 1, &[_][*c]const u8{fragment_shader_code}, null);
    c.glCompileShader(fragment_shader);
    success = 0;
    c.glGetShaderiv(fragment_shader, c.GL_COMPILE_STATUS, &success);
    if (success == 0) {
        var info_log: [512:0]u8 = undefined;
        c.glGetShaderInfoLog(fragment_shader, info_log.len, null, &info_log);
        std.log.err("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n{s}\n", .{@as([:0]const u8, &info_log)});
    }

    g_mem.shader_program = c.glCreateProgram();
    c.glAttachShader(g_mem.shader_program, vertex_shader);
    c.glAttachShader(g_mem.shader_program, fragment_shader);
    c.glLinkProgram(g_mem.shader_program);

    success = 0;
    c.glGetProgramiv(g_mem.shader_program, c.GL_LINK_STATUS, &success);
    if (success == 0) {
        var info_log: [512:0]u8 = undefined;
        c.glGetProgramInfoLog(g_mem.shader_program, info_log.len, null, &info_log);
        std.log.err("ERROR::SHADER::PROGRAM::LINK_FAILED\n{s}\n", .{@as([:0]const u8, &info_log)});
    }
}

const vertex_shader_code = @embedFile("shaders/vert.glsl");
const fragment_shader_code = @embedFile("shaders/frag.glsl");
const vertices = [_]f32{
    -0.5, -0.5, 0.0,
    0.5,  -0.5, 0.0,
    0,    0.5,  0.0,
};

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
                        g_init.width = event.window.data1;
                        g_init.height = event.window.data2;

                        c.glViewport(0, 0, g_init.width, g_init.height);
                    },
                    else => {},
                }
            },
            else => {},
        }
        g_mem.counter += 1;

        c.glClearColor(0.0, 0.0, 0.0, 1.0);
        c.glClear(c.GL_COLOR_BUFFER_BIT);

        c.glUseProgram(g_mem.shader_program);
        c.glBindVertexArray(g_mem.triangle_vao);
        c.glDrawArrays(c.GL_TRIANGLES, 0, 3);

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
    g_init_exists = false;
    c.SDL_Quit();
}

export fn game_hot_reload(init_memory: ?*anyopaque, gmemory: ?*anyopaque) void {
    std.log.debug("game_hot_reload {any} {any}\n", .{ init_memory, gmemory });
    if (init_memory) |init_mem| {
        g_init = @alignCast(@ptrCast(init_mem));
        g_init_exists = true;
    }
    if (gmemory) |gmem| {
        g_mem = @alignCast(@ptrCast(gmem));
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
