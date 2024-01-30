const std = @import("std");
const c = @import("c.zig");
const builtin = @import("builtin");

fn sdl_try(result: c_int) !void {
    if (result < 0) {
        std.log.err("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLError;
    }
}

const game_lib_basename = "learnopengl";
const game_lib_name: [:0]const u8 = builtin.target.libPrefix() ++ game_lib_basename ++ builtin.target.dynamicLibSuffix();

var game_api: GameAPI = undefined;

pub fn main() !void {
    var global_alloc = std.heap.LoggingAllocator(.debug, .err).init(std.heap.c_allocator);
    var allocator = global_alloc.allocator();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const temp_allocator = arena.allocator();
    _ = temp_allocator;

    const game_lib_path = try getGameLibPath(allocator);

    try sdl_try(c.SDL_Init(c.SDL_INIT_EVERYTHING));
    defer c.SDL_Quit();

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
    defer c.SDL_GL_DeleteContext(context);

    _ = c.gladLoadGLLoader(c.SDL_GL_GetProcAddress);

    std.log.debug("OpenGL Version: {}.{}", .{ c.GLVersion.major, c.GLVersion.minor });

    var modified = try getFileModifiedZ(game_lib_path);
    game_api = try loadGameAPI(allocator, game_lib_path);

    game_api.game_init(&allocator);
    defer {
        game_api.game_shutdown();
        game_api.deinit(allocator);
    }

    var exit = false;

    while (!exit) {
        _ = arena.reset(.retain_capacity);

        const new_modified = try getFileModifiedZ(game_lib_path);

        if (new_modified != modified) {
            modified = new_modified;
            var new_game_api = try loadGameAPI(allocator, game_lib_path);

            if (game_api.game_memory_size() == new_game_api.game_memory_size()) {
                std.log.debug("Hot reload with state!\n", .{});
                const game_memory = game_api.game_memory();
                game_api.deinit(allocator);
                new_game_api.game_hot_reload(game_memory);
                game_api = new_game_api;
            } else {
                std.log.debug("Hot reload with shutdown!\n", .{});
                game_api.game_shutdown();
                game_api.deinit(allocator);
                game_api = new_game_api;
                game_api.game_init(&allocator);
            }
        }

        exit = !game_api.game_update();

        c.SDL_GL_SwapWindow(window);
        c.SDL_Delay(1);
    }
}

fn getFileModifiedZ(path: [:0]const u8) !i128 {
    var lib_file = try std.fs.openFileAbsoluteZ(path, .{});
    defer lib_file.close();
    var lib_file_meta = try lib_file.metadata();

    return lib_file_meta.modified();
}

fn getGameLibPath(allocator: std.mem.Allocator) ![:0]const u8 {
    const base_path = std.mem.span(c.SDL_GetBasePath());
    const game_lib_path = try std.fs.path.joinZ(allocator, &.{ base_path, game_lib_name });

    return game_lib_path;
}

var game_version: usize = 0;

fn loadGameAPI(arena: std.mem.Allocator, game_lib_path: [:0]const u8) !GameAPI {
    game_version += 1;
    const base_path = std.mem.span(c.SDL_GetBasePath());
    const temp_lib_name = try std.fmt.allocPrintZ(
        arena,
        builtin.target.libPrefix() ++ "{s}_{}" ++ builtin.target.dynamicLibSuffix(),
        .{ game_lib_basename, game_version },
    );
    const temp_lib_path = try std.fs.path.joinZ(arena, &.{ base_path, temp_lib_name });
    std.log.debug("Copying game dll from {s} to {s}\n", .{ game_lib_path, temp_lib_path });
    try std.fs.copyFileAbsolute(game_lib_path, temp_lib_path, .{});

    std.log.debug("Loading game lib at path {s}\n", .{temp_lib_path});
    const dll = c.SDL_LoadObject(temp_lib_path);

    if (dll == null) {
        std.log.debug("SDL Error: {s}", .{c.SDL_GetError()});
        return error.SDLDLLLoadFailed;
    }

    return try GameAPI.init(dll, temp_lib_path);
}

const GameAPI = struct {
    game_init: *const fn (*std.mem.Allocator) callconv(.C) void,
    game_update: *const fn () callconv(.C) bool,
    game_shutdown: *const fn () callconv(.C) void,
    game_memory_size: *const fn () callconv(.C) usize,
    game_memory: *const fn () callconv(.C) *anyopaque,
    game_hot_reload: *const fn (*anyopaque) callconv(.C) void,
    dll: ?*anyopaque,
    path: [:0]const u8,

    pub fn init(dll: ?*anyopaque, path: [:0]const u8) !GameAPI {
        return GameAPI{
            .game_init = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_init") orelse return error.MissingGameInit)),
            .game_update = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_update") orelse return error.MissingGameUpdate)),
            .game_shutdown = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_shutdown") orelse return error.MissingGameShutdown)),
            .game_memory_size = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_memory_size") orelse return error.MissingGameMemorySize)),
            .game_memory = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_memory") orelse return error.MissingGameMemory)),
            .game_hot_reload = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_hot_reload") orelse return error.MissingGameHotReload)),
            .dll = dll,
            .path = path,
        };
    }

    pub fn deinit(self: *GameAPI, allocator: std.mem.Allocator) void {
        c.SDL_UnloadObject(self.dll);
        std.fs.deleteFileAbsolute(self.path) catch |err| {
            std.log.debug("failed to delete temp game dll {}", .{err});
        };
        allocator.free(self.path);
    }
};
