const std = @import("std");
const c = @import("c.zig");

pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    counter: i32 = 0,
};

var g_mem: *GameMemory = undefined;

export fn game_init(global_allocator: *std.mem.Allocator) void {
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
            else => {},
        }
        g_mem.counter += 1;

        c.glClearColor(0.3, 0.6, 0.3, 1.0);
        c.glClear(c.GL_COLOR_BUFFER_BIT);
    }

    return true;
}

export fn game_shutdown() void {
    g_mem.global_allocator.destroy(g_mem);
}

export fn game_hot_reload(memory: *anyopaque) void {
    g_mem = @alignCast(@ptrCast(memory));
}

export fn game_memory() *anyopaque {
    return @ptrCast(g_mem);
}

export fn game_memory_size() usize {
    return @sizeOf(GameMemory);
}
