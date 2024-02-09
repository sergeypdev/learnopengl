const std = @import("std");
const sdl = @import("sdl.zig");
const builtin = @import("builtin");
const fs_utils = @import("fs/utils.zig");

const game_lib_basename = "learnopengl";
const game_lib_name: [:0]const u8 = builtin.target.libPrefix() ++ game_lib_basename ++ builtin.target.dynamicLibSuffix();

var game_api: GameAPI = undefined;

// pub extern "winmm" fn timeBeginPeriod(period: c_uint) c_uint;

pub fn main() !void {
    // _ = timeBeginPeriod(1);
    var global_alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = global_alloc.allocator();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const temp_allocator = arena.allocator();
    _ = temp_allocator;

    const game_lib_path = try getGameLibPath(allocator);

    var modified = try fs_utils.getFileModifiedZ(game_lib_path);
    game_api = try loadGameAPI(allocator, game_lib_path);

    game_api.game_init_window(&allocator);

    game_api.game_init(&allocator);
    defer {
        game_api.game_shutdown();
        game_api.deinit(allocator);
    }

    var exit = false;

    while (!exit) {
        _ = arena.reset(.retain_capacity);

        const new_modified = try fs_utils.getFileModifiedZ(game_lib_path);

        if (new_modified != modified) {
            modified = new_modified;
            var new_game_api = try loadGameAPI(allocator, game_lib_path);

            var recreate_state = false;
            var recreate_window = false;

            const game_init_memory = game_api.game_init_memory();
            const game_memory = game_api.game_memory();

            if (game_api.game_memory_size() != new_game_api.game_memory_size()) {
                recreate_state = true;
            }
            if (game_api.game_init_memory_size() != new_game_api.game_init_memory_size()) {
                recreate_window = true;
            }

            if (recreate_state) {
                game_api.game_shutdown();
            }
            if (recreate_window) {
                game_api.game_shutdown_window();
            }

            if (recreate_window) {
                new_game_api.game_init_window(&allocator);
            } else {
                new_game_api.game_hot_reload(game_init_memory, null);
            }
            if (recreate_state) {
                new_game_api.game_init(&allocator);
            } else {
                new_game_api.game_hot_reload(null, game_memory);
            }

            game_api.deinit(allocator);
            game_api = new_game_api;
        }

        exit = !game_api.game_update();
    }
}

fn getGameLibPath(allocator: std.mem.Allocator) ![:0]const u8 {
    const base_path = std.mem.span(sdl.SDL_GetBasePath());
    const game_lib_path = try std.fs.path.joinZ(allocator, &.{ base_path, game_lib_name });

    return game_lib_path;
}

var game_version: usize = 0;

fn loadGameAPI(arena: std.mem.Allocator, game_lib_path: [:0]const u8) !GameAPI {
    game_version += 1;
    const base_path = std.mem.span(sdl.SDL_GetBasePath());
    const temp_lib_name = try std.fmt.allocPrintZ(
        arena,
        builtin.target.libPrefix() ++ "{s}_{}" ++ builtin.target.dynamicLibSuffix(),
        .{ game_lib_basename, game_version },
    );
    const temp_lib_path = try std.fs.path.joinZ(arena, &.{ base_path, temp_lib_name });
    std.log.debug("Copying game dll from {s} to {s}\n", .{ game_lib_path, temp_lib_path });
    try std.fs.copyFileAbsolute(game_lib_path, temp_lib_path, .{});

    std.log.debug("Loading game lib at path {s}\n", .{temp_lib_path});
    const dll = sdl.SDL_LoadObject(temp_lib_path);

    if (dll == null) {
        std.log.debug("SDL Error: {s}", .{sdl.SDL_GetError()});
        return error.SDLDLLLoadFailed;
    }

    return try GameAPI.init(dll, temp_lib_path);
}

const GameAPI = struct {
    game_init_window: *const fn (*std.mem.Allocator) callconv(.C) void,
    game_init: *const fn (*std.mem.Allocator) callconv(.C) void,
    game_update: *const fn () callconv(.C) bool,
    game_shutdown: *const fn () callconv(.C) void,
    game_shutdown_window: *const fn () callconv(.C) void,
    game_memory_size: *const fn () callconv(.C) usize,
    game_init_memory_size: *const fn () callconv(.C) usize,
    game_memory: *const fn () callconv(.C) *anyopaque,
    game_init_memory: *const fn () callconv(.C) *anyopaque,
    game_hot_reload: *const fn (?*anyopaque, ?*anyopaque) callconv(.C) void,
    dll: ?*anyopaque,
    path: [:0]const u8,

    pub fn init(dll: ?*anyopaque, path: [:0]const u8) !GameAPI {
        return GameAPI{
            .game_init_window = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_init_window") orelse return error.MissingGameInitWindow)),
            .game_init = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_init") orelse return error.MissingGameInit)),
            .game_update = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_update") orelse return error.MissingGameUpdate)),
            .game_shutdown = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_shutdown") orelse return error.MissingGameShutdown)),
            .game_shutdown_window = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_shutdown_window") orelse return error.MissingGameShutdownWindow)),
            .game_memory_size = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_memory_size") orelse return error.MissingGameMemorySize)),
            .game_init_memory_size = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_init_memory_size") orelse return error.MissingGameInitMemorySize)),
            .game_memory = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_memory") orelse return error.MissingGameMemory)),
            .game_init_memory = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_init_memory") orelse return error.MissingGameInitMemory)),
            .game_hot_reload = @alignCast(@ptrCast(sdl.SDL_LoadFunction(dll, "game_hot_reload") orelse return error.MissingGameHotReload)),
            .dll = dll,
            .path = path,
        };
    }

    pub fn deinit(self: *GameAPI, allocator: std.mem.Allocator) void {
        sdl.SDL_UnloadObject(self.dll);
        std.fs.deleteFileAbsolute(self.path) catch |err| {
            std.log.debug("failed to delete temp game dll {}", .{err});
        };
        allocator.free(self.path);
    }
};
