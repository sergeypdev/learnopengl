const std = @import("std");
const c = @import("sdl.zig");
const Render = @import("Render.zig");
const AssetManager = @import("AssetManager.zig");
const World = @import("entity.zig").World;

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;

pub var g_init_exists = false;
pub var g_init: *InitMemory = undefined;
pub var g_mem: *GameMemory = undefined;
pub var g_assetman: *AssetManager = undefined;

pub const DEFAULT_WIDTH = 800;
pub const DEFAULT_HEIGHT = 600;

pub const InitMemory = struct {
    global_allocator: std.mem.Allocator,
    window: *c.SDL_Window,
    context: ?*anyopaque,
    width: c_int,
    height: c_int,
    fullscreen: bool = false,
    vsync: bool = false,
    syswm_info: c.SDL_SysWMinfo = .{},
};
pub const GameMemory = struct {
    global_allocator: std.mem.Allocator,
    frame_fba: std.heap.FixedBufferAllocator,
    assetman: AssetManager,
    render: Render,
    performance_frequency: u64 = 0,
    last_frame_time: u64 = 0,
    delta_time: f32 = 0.0000001,
    rotation: f32 = 0,
    input_state: InputState = .{},
    free_cam: FreeLookCamera = .{},
    mouse_focus: bool = false,
    world: World,
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

    camera: Render.Camera = .{},

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

        self.camera.view_mat = Mat4.lookAt(self.pos, self.pos.add(forward), Vec3.up());
    }
};
