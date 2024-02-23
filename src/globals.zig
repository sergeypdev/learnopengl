const std = @import("std");
const c = @import("sdl.zig");
const AssetManager = @import("AssetManager.zig");
const Render = @import("Render.zig");

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

pub const MAX_ENTITIES = 1024;

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

        _local_dirty: bool = true,
        _local: Mat4 = Mat4.identity(),

        _global_dirty: bool = true,
        _global: Mat4 = Mat4.identity(),

        pub fn dirty(self: *Transform) void {
            self._local_dirty = true;
            self._global_dirty = true;
        }

        pub fn setPos(self: *Transform, new_pos: Vec3) void {
            self.pos = new_pos;
            self.dirty();
        }

        pub fn translate(self: *Transform, by: Vec3) void {
            self.pos = self.pos.add(by);
            self.dirty();
        }
    };

    pub const Mesh = struct {
        handle: AssetManager.Handle.Mesh = .{},
        material: Render.Material = .{},
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
    parent: ?EntityHandle = null,
    transform: Transform = .{},
    mesh: Mesh = .{},
    point_light: PointLight = .{},
    rotate: Rotate = .{},

    pub fn localMatrix(self: *Entity) *const Mat4 {
        if (self.transform._local_dirty) {
            self.transform._local = Mat4.recompose(self.transform.pos, self.transform.rot, self.transform.scale);
            self.transform._local_dirty = false;
        }
        return &self.transform._local;
    }

    pub fn globalMatrix(self: *Entity, world: *World) *const Mat4 {
        // TODO: think how to reduce pointer chasing
        if (self.parent) |parent_ent| {
            if (world.getEntity(parent_ent)) |parent| {
                if (parent.transform._global_dirty or self.transform._global_dirty) {
                    self.transform._global = parent.globalMatrix(world).mul(self.localMatrix().*);
                    self.transform._global_dirty = false;
                }

                return &self.transform._global;
            }
        }

        return self.localMatrix();
    }
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
    render: Render,
    performance_frequency: u64 = 0,
    last_frame_time: u64 = 0,
    delta_time: f32 = 0.0000001,
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
