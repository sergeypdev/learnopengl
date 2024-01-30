const std = @import("std");
const c = @import("c.zig");
const builtin = @import("builtin");

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

    var modified = try getFileModifiedZ(game_lib_path);
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

        const new_modified = try getFileModifiedZ(game_lib_path);

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
            .game_init_window = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_init_window") orelse return error.MissingGameInitWindow)),
            .game_init = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_init") orelse return error.MissingGameInit)),
            .game_update = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_update") orelse return error.MissingGameUpdate)),
            .game_shutdown = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_shutdown") orelse return error.MissingGameShutdown)),
            .game_shutdown_window = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_shutdown_window") orelse return error.MissingGameShutdownWindow)),
            .game_memory_size = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_memory_size") orelse return error.MissingGameMemorySize)),
            .game_init_memory_size = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_init_memory_size") orelse return error.MissingGameInitMemorySize)),
            .game_memory = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_memory") orelse return error.MissingGameMemory)),
            .game_init_memory = @alignCast(@ptrCast(c.SDL_LoadFunction(dll, "game_init_memory") orelse return error.MissingGameInitMemory)),
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
