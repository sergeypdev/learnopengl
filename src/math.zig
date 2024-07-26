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

// Using DirectX/Vulkan NDC coordinates
pub const ndc_box_corners = [8]Vec3{
    Vec3.new(-1, -1, 0),
    Vec3.new(-1, -1, 1),
    Vec3.new(-1, 1, 0),
    Vec3.new(-1, 1, 1),
    Vec3.new(1, -1, 0),
    Vec3.new(1, -1, 1),
    Vec3.new(1, 1, 0),
    Vec3.new(1, 1, 1),
};

pub const Plane = struct {
    // x, y, z - normal, w - negative distance
    nd: Vec4 = Vec4.up(),

    pub fn new(normal_d: Vec4) Plane {
        const scale = 1.0 / normal_d.toVec3().length();
        return .{ .nd = normal_d.scale(scale) };
    }

    pub fn fromNormalDistance(norm: Vec3, dist: f32) Plane {
        return Plane.new(norm.toVec4(-dist));
    }

    pub inline fn normal(self: Plane) Vec3 {
        return self.nd.toVec3();
    }

    pub inline fn distance(self: Plane) f32 {
        return -self.nd.w();
    }

    pub fn point(self: Plane) Vec3 {
        return self.nd.toVec3().scale(self.nd.w());
    }

    pub fn transform(self: *const Plane, matrix: *const Mat4) Plane {
        var p = self.point().toVec4(1);
        var n = self.normal().toVec4(0);

        p = matrix.mulByVec4(p);
        p = p.scale(1.0 / p.w());
        n = matrix.mulByVec4(n).norm();

        n.wMut().* = p.toVec3().dot(n.toVec3());

        return .{ .nd = n };
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
        var min = Vec3.new(std.math.floatMax(f32), std.math.floatMax(f32), std.math.floatMax(f32));
        var max = Vec3.new(std.math.floatMin(f32), std.math.floatMin(f32), std.math.floatMin(f32));

        inline for (box_corners) |corner| {
            const corner_pos = matrix.mulByVec4(self.origin.add(self.extents.mul(corner)).toVec4(1));
            const corner_pos3 = corner_pos.toVec3();
            min = corner_pos3.min(min);
            max = corner_pos3.max(max);
        }

        return AABB.fromMinMax(min, max);
    }

    pub fn toSphere(self: *const AABB) BoundingSphere {
        return BoundingSphere{
            .origin = self.origin,
            .radius = self.extents.length(),
        };
    }
};

pub const BoundingSphere = struct {
    origin: Vec3 = Vec3.zero(),
    radius: f32 = 0,

    pub fn new(origin: Vec3, radius: f32) BoundingSphere {
        return BoundingSphere{ .origin = origin, .radius = radius };
    }
};

pub const Frustum = struct {
    // Plane normals
    // top
    // right
    // bottom
    // left
    // near
    // far
    planes: [6]Plane = std.mem.zeroes([6]Plane),

    pub const PlaneSide = enum {
        Top,
        Right,
        Bottom,
        Left,
        Near,
        Far,
    };

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
        const near = row3;
        const far = row4.sub(row3);

        return .{
            .planes = .{
                Plane.new(top),
                Plane.new(right),
                Plane.new(bottom),
                Plane.new(left),
                Plane.new(near),
                Plane.new(far),
            },
        };
    }

    pub fn getPlane(self: *const Frustum, side: PlaneSide) *const Plane {
        return &self.planes[@intFromEnum(side)];
    }

    pub fn getNearDist(self: *const Frustum) f32 {
        return self.getPlane(.Near).distance();
    }

    pub fn rangeZ(self: *const Frustum) f32 {
        return self.getPLane(.Far).point().sub(self.getPlane(.Near).point()).dot(self.getPlane(.Near).normal());
    }

    pub fn transform(self: *const Frustum, matrix: *const Mat4) Frustum {
        const new_planes = self.planes;
        for (new_planes) |*plane| {
            plane.* = plane.transform(matrix);
        }
        return Frustum{
            .planes = new_planes,
        };
    }

    pub fn intersectPoint(self: *const Frustum, point: Vec3) bool {
        return !(self.top.isUnder(point) or self.right.isUnder(point) or self.bottom.isUnder(point) or self.left.isUnder(point) or self.near.isUnder(point) or self.far.isUnder(point));
    }

    fn intersectAABBInternal(self: *const Frustum, aabb: AABB, comptime skip_near: bool) bool {
        for (0..6) |i| {
            if (skip_near and i == @intFromEnum(PlaneSide.Near)) continue;

            const plane = self.planes[i];
            const nx = plane.normal().x() > 0;
            const ny = plane.normal().y() > 0;
            const nz = plane.normal().z() > 0;

            const min = aabb.origin.sub(aabb.extents);
            const max = aabb.origin.add(aabb.extents);

            // TODO: vectorize
            const dot = (plane.normal().x() * if (nx) max.x() else min.x()) + (plane.normal().y() * if (ny) max.y() else min.y()) + (plane.normal().z() * if (nz) max.z() else min.z());

            if (dot < plane.distance()) {
                return false;
            }

            // const dot2 = (plane.normal().x() * if (nx) min.x else max.x) + (plane.normal().y() * if (ny) min.y else max.y) + (plane.normal().z() * if (nz) min.z else max.z);
            // planes have unit-length normal, offset = -dot(normal, point on plane)
            // 	const Plane3& plane = planes[i];
            // 	Index nx = plane.normal.x > Real(0);
            // 	Index ny = plane.normal.y > Real(0);
            // 	Index nz = plane.normal.z > Real(0);
            //
            // 	// getMinMax(): 0 = return min coordinate. 1 = return max.
            // 	Real dot = (plane.normal.x*box.getMinMax(nx).x) + (plane.normal.y*box.getMinMax(ny).y) + (plane.normal.z*box.getMinMax(nz).z);
            //
            // 	if ( dot < -plane.offset )
            // 		return OUTSIDE;
            //
            // 	Real dot2 = (plane.normal.x*box.getMinMax(1-nx).x) + (plane.normal.y*box.getMinMax(1-ny).y) + (plane.normal.z*box.getMinMax(1-nz).z);
            //
            // 	if ( dot2 <= -plane.offset )
            // 		result = INTERSECTS;
        }

        return true;
    }

    pub fn intersectAABB(self: *const Frustum, aabb: AABB) bool {
        return self.intersectAABBInternal(aabb, false);
    }

    pub fn intersectAABBSkipNear(self: *const Frustum, aabb: AABB) bool {
        return self.intersectAABBInternal(aabb, true);
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

/// Formulas from D3DXMatrixPerspectiveFovRH
/// Maps z to [0, 1]
pub fn perspective(fovy_in_degrees: f32, aspect_ratio: f32, z_near: f32, z_far: f32) Mat4 {
    var result = Mat4.identity();

    const f = 1 / @tan(za.toRadians(fovy_in_degrees) * 0.5);

    result.data[0][0] = f / aspect_ratio;
    result.data[1][1] = f;
    result.data[2][2] = z_far / (z_near - z_far);
    result.data[2][3] = -1;
    result.data[3][2] = z_far * z_near / (z_near - z_far);
    result.data[3][3] = 0;

    return result;
}

/// Formulas from D3DXMatrixOrthoOffCenterRH
/// Maps z to [0, 1]
pub fn orthographic(left: f32, right: f32, bottom: f32, top: f32, z_near: f32, z_far: f32) Mat4 {
    var result = Mat4.zero();

    result.data[0][0] = 2 / (right - left);
    result.data[1][1] = 2 / (top - bottom);
    result.data[2][2] = 1 / (z_near - z_far);
    result.data[3][3] = 1;

    result.data[3][0] = (left + right) / (left - right);
    result.data[3][1] = (bottom + top) / (bottom - top);
    result.data[3][2] = z_near / (z_near - z_far);

    return result;
}
