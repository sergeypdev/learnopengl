const std = @import("std");
const Handle = @import("assets").Handle;
const formats = @import("formats.zig");
const Material = formats.Material;
const Scene = formats.Scene;

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;

pub const MAX_ENTITIES = 1024 * 10;

pub const Entity = struct {
    pub const Flags = packed struct {
        active: bool = false,
        mesh: bool = false,
        point_light: bool = false,
        dir_light: bool = false,
        rotate: bool = false,
        _pad: u3 = 0, // make it abi sized
    };

    pub const Transform = extern struct {
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

        pub fn rotate(self: *Transform, axis: Vec3, angle: f32) void {
            self.rot = Quat.fromAxis(angle, axis).mul(self.rot);
            self.dirty();
        }
    };

    pub const Mesh = extern struct {
        handle: Handle.Mesh = .{},
        material: Material = .{}, // used if override_material == true
        override_material: bool = false,
    };
    pub const Light = extern struct {
        color_intensity: Vec4 = Vec4.one(), // x, y, z - color, w - intensity

        /// Returns Vec3 color multiplied by intensity (w)
        pub fn premultipliedColor(self: *const Light) Vec3 {
            const col = self.color_intensity;
            return Vec3.new(col.x(), col.y(), col.z()).scale(col.w());
        }
    };
    pub const PointLight = extern struct {
        radius: f32 = std.math.floatEps(f32), // should never be 0 or bad things happen
    };
    pub const Rotate = extern struct {
        axis: Vec3 = Vec3.up(),
        rate: f32 = 0, // deg/s
    };

    /// Serializable entity data
    pub const Data = extern struct {
        flags: Flags = .{},
        transform: Transform = .{},
        mesh: Mesh = .{},
        light: Light = .{},
        point_light: PointLight = .{},
        rotate: Rotate = .{},
    };

    // Entity list and handle management
    idx: u32 = 0,
    gen: u32 = 0,
    // Free list
    next: ?*Entity = null,

    parent: ?EntityHandle = null,

    data: Data = .{},

    pub fn setParent(self: *Entity, parent: ?EntityHandle) void {
        self.parent = parent;
        self.data.transform.dirty();
    }

    pub fn localMatrix(self: *Entity) *const Mat4 {
        if (self.data.transform._local_dirty) {
            self.data.transform._local = Mat4.recompose(self.data.transform.pos, self.data.transform.rot, self.data.transform.scale);
            self.data.transform._local_dirty = false;
        }
        return &self.data.transform._local;
    }

    pub fn globalMatrix(self: *Entity, world: *World) *const Mat4 {
        // TODO: think how to reduce pointer chasing
        if (self.parent) |parent_ent| {
            if (world.getEntity(parent_ent)) |parent| {
                if (parent.data.transform._global_dirty or self.data.transform._global_dirty) {
                    self.data.transform._global = parent.globalMatrix(world).mul(self.localMatrix().*);
                    self.data.transform._global_dirty = false;
                }

                return &self.data.transform._global;
            }
        }

        return self.localMatrix();
    }
};

pub const EntityHandle = packed struct {
    idx: u32 = 0,
    gen: u32 = 0,

    /// Returns handle with 0 idx and gen.
    /// 0 gen is invalid, valid generations start from 0
    pub fn invalid() EntityHandle {
        return EntityHandle{};
    }
};

pub const EntityCreateResult = struct {
    handle: EntityHandle,
    ptr: *Entity,
};

pub const World = struct {
    frame_arena: std.mem.Allocator,
    entities: [MAX_ENTITIES]Entity = [_]Entity{.{}} ** MAX_ENTITIES,
    entity_count: usize = 0,
    free_entity: ?*Entity = null,

    pub fn addEntity(self: *World, data: Entity.Data) EntityCreateResult {
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
        ent.data = data;
        // TODO: handle wrapping
        ent.gen = gen + 1;
        ent.idx = idx;
        ent.data.flags.active = true;
        self.free_entity = next;

        return EntityCreateResult{
            .handle = EntityHandle{ .idx = idx, .gen = ent.gen },
            .ptr = ent,
        };
    }

    /// Spawns a scene and returns a hand to the root entity
    pub fn createScene(self: *World, scene: Scene) EntityHandle {
        if (scene.entities.len == 0) {
            return EntityHandle.invalid();
        }

        const handles = self.frame_arena.alloc(EntityHandle, scene.entities.len) catch @panic("OOM"); // not handling error, this is unrecoverable

        for (0.., handles, scene.entities, scene.parents) |i, *out_handle, *ent, parent| {
            const res = self.addEntity(ent.*);
            out_handle.* = res.handle;

            if (parent >= 0) {
                std.debug.assert(parent < i);
                res.ptr.parent = handles[@intCast(parent)];
            }
        }

        return handles[0];
    }

    pub fn getEntity(self: *World, handle: EntityHandle) ?*Entity {
        // Gen 0 is always invalid
        if (handle.gen == 0) return null;

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
