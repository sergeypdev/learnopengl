const std = @import("std");

const za = @import("zalgebra");
const Vec2 = za.Vec2;
const Vec3 = za.Vec3;
const Vec4 = za.Vec4;
const Mat4 = za.Mat4;
const Quat = za.Quat;

pub const box_corners = [8]Vec3{
    Vec3.new(-1, -1, -1),
    Vec3.new(-1, -1, 1),
    Vec3.new(-1, 1, -1),
    Vec3.new(-1, 1, 1),
    Vec3.new(1, -1, -1),
    Vec3.new(1, -1, 1),
    Vec3.new(1, 1, -1),
    Vec3.new(1, 1, 1),
};

pub const Plane = struct {
    // x, y, z - normal, w - distance
    nd: Vec4 = Vec4.up(),

    pub fn new(normal_d: Vec4) Plane {
        const scale = 1.0 / normal_d.toVec3().length();
        return .{ .nd = normal_d.scale(scale) };
    }

    pub fn transform(self: *const Plane, matrix: *const Mat4) Plane {
        var point = Vec4.fromVec3(self.nd.toVec3().scale(self.nd.w()), 1);
        var normal = Vec4.fromVec3(self.nd.toVec3(), 0);

        point = matrix.mulByVec4(point);
        point = point.scale(1.0 / point.w());
        normal = matrix.mulByVec4(normal).norm();

        normal.wMut().* = point.toVec3().dot(normal.toVec3());

        return .{ .nd = normal };
    }

    pub fn isUnder(self: *const Plane, p: Vec3) bool {
        return self.nd.dot(p.toVec4(1)) < 0;
    }
};

pub const AABB = struct {
    origin: Vec3 = Vec3.zero(),
    extents: Vec3 = Vec3.zero(),

    pub fn fromMinMax(min: Vec3, max: Vec3) AABB {
        const extents = max.sub(min).scale(0.5);
        const origin = min.add(extents);

        return AABB{ .origin = origin, .extents = extents };
    }

    // TODO: optimize
    pub fn transform(self: *const AABB, matrix: Mat4) AABB {
        var min = Vec3.zero();
        var max = Vec3.zero();

        inline for (box_corners) |corner| {
            const corner_pos = matrix.mulByVec4(self.origin.add(self.extents.mul(corner)).toVec4(1));
            const corner_pos3 = corner_pos.scale(1 / corner_pos.w()).toVec3();
            min = Vec3.new(@min(corner_pos3.x(), min.x()), @min(corner_pos3.y(), min.y()), @min(corner_pos3.z(), min.z()));
            max = Vec3.new(@max(corner_pos3.x(), max.x()), @max(corner_pos3.y(), max.y()), @max(corner_pos3.z(), max.z()));
        }

        return AABB.fromMinMax(min, max);
    }
};

pub const Frustum = struct {
    // Plane normals
    top: Plane,
    right: Plane,
    bottom: Plane,
    left: Plane,
    near: Plane,
    far: Plane,

    /// Extracts frustum planes from matrices using Gribb-Hartmann method
    /// If you pass in a projection matrix planes will be in view space.
    /// If you pass in a view-projection matrix planes will be in world space.
    /// If you pass in a model-view-projection matrix planes will be in model space.
    pub fn new(mat: Mat4) Frustum {
        const row1 = Vec4.new(mat.data[0][0], mat.data[1][0], mat.data[2][0], mat.data[3][0]);
        const row2 = Vec4.new(mat.data[0][1], mat.data[1][1], mat.data[2][1], mat.data[3][1]);
        const row3 = Vec4.new(mat.data[0][2], mat.data[1][2], mat.data[2][2], mat.data[3][2]);
        const row4 = Vec4.new(mat.data[0][3], mat.data[1][3], mat.data[2][3], mat.data[3][3]);

        const left = row4.add(row1);
        const right = row4.sub(row1);
        const bottom = row4.add(row2);
        const top = row4.sub(row2);
        const near = row4.add(row3);
        const far = row4.sub(row3);

        return .{
            .top = Plane.new(top),
            .right = Plane.new(right),
            .bottom = Plane.new(bottom),
            .left = Plane.new(left),
            .near = Plane.new(near),
            .far = Plane.new(far),
        };
    }

    pub fn transform(self: *const Frustum, matrix: *const Mat4) Frustum {
        return Frustum{
            .top = self.top.transform(matrix),
            .right = self.right.transform(matrix),
            .bottom = self.bottom.transform(matrix),
            .left = self.left.transform(matrix),
            .near = self.near.transform(matrix),
            .far = self.far.transform(matrix),
        };
    }

    pub fn intersectPoint(self: *const Frustum, point: Vec3) bool {
        return !(self.top.isUnder(point) or self.right.isUnder(point) or self.bottom.isUnder(point) or self.left.isUnder(point) or self.near.isUnder(point) or self.far.isUnder(point));
    }

    pub fn intersectAABB(self: *const Frustum, aabb: AABB) bool {
        var outside_top_plane = true;
        var outside_bottom_plane = true;
        var outside_left_plane = true;
        var outside_right_plane = true;
        var outside_near_plane = true;
        var outside_far_plane = true;
        inline for (box_corners) |corner| {
            const p = aabb.origin.add(aabb.extents.mul(corner));

            outside_top_plane = outside_top_plane and self.top.isUnder(p);
            outside_bottom_plane = outside_bottom_plane and self.bottom.isUnder(p);
            outside_left_plane = outside_left_plane and self.left.isUnder(p);
            outside_right_plane = outside_right_plane and self.right.isUnder(p);
            outside_near_plane = outside_near_plane and self.near.isUnder(p);
            outside_far_plane = outside_far_plane and self.far.isUnder(p);
        }

        return !(outside_left_plane or outside_right_plane or outside_bottom_plane or outside_top_plane or outside_near_plane or outside_far_plane);
    }
};

pub fn checkAABBIntersectionNDC(aabb: *const AABB, mvp: *const Mat4) bool {
    var outside_left_plane = true;
    var outside_right_plane = true;
    var outside_top_plane = true;
    var outside_bottom_plane = true;
    var outside_near_plane = true;
    var outside_far_plane = true;

    for (box_corners) |corner| {
        const p3 = aabb.origin.add(aabb.extents.mul(corner));
        var p_ndc = mvp.mulByVec4(p3.toVec4(1));
        p_ndc = p_ndc.scale(1.0 / p_ndc.w());

        outside_left_plane = outside_left_plane and p_ndc.x() < -1;
        outside_right_plane = outside_right_plane and p_ndc.x() > 1;

        outside_bottom_plane = outside_bottom_plane and p_ndc.y() < -1;
        outside_top_plane = outside_top_plane and p_ndc.y() > 1;

        outside_near_plane = outside_near_plane and p_ndc.z() < -1;
        outside_far_plane = outside_far_plane and p_ndc.z() > 1;
    }

    return !(outside_left_plane or outside_right_plane or outside_bottom_plane or outside_top_plane or outside_near_plane or outside_far_plane);
}