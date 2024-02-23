const std = @import("std");

pub const AssetType = enum {
    Scene,
    Mesh,
    Shader,
    ShaderProgram,
    Texture,

    pub fn pluralName(self: AssetType) []const u8 {
        return switch (self) {
            .Scene => "Scenes",
            .Mesh => "Meshes",
            .Shader => "Shaders",
            .ShaderProgram => "ShaderPrograms",
            .Texture => "Textures",
        };
    }

    pub fn ext(self: AssetType) []const u8 {
        return switch (self) {
            .Scene => "scn",
            .Mesh => "mesh",
            .Shader => "glsl",
            .ShaderProgram => "prog",
            .Texture => "tex",
        };
    }
};

pub const AssetPath = union(enum) {
    simple: []const u8,
    nested: struct {
        path: []const u8,
        sub_path: []const u8,
    },

    pub fn hash(self: AssetPath) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, self.getPath(), .Deep);
        std.hash.autoHashStrat(&hasher, self.getSubPath(), .Deep);
        return hasher.final();
    }

    pub fn getPath(self: AssetPath) []const u8 {
        return switch (self) {
            .simple => |path| path,
            .nested => |nested| nested.path,
        };
    }

    pub fn getSubPath(self: AssetPath) ?[]const u8 {
        return switch (self) {
            .nested => |nested| nested.sub_path,
            else => null,
        };
    }

    pub inline fn strLen(self: AssetPath) usize {
        return switch (self) {
            .simple => |path| path.len,
            .nested => |nested| return nested.path.len + nested.sub_path.len + 1,
        };
    }

    pub fn writeString(self: AssetPath, writer: anytype) !void {
        switch (self) {
            .simple => |path| {
                try writer.writeAll(path);
            },
            .nested => |nested| {
                try writer.writeAll(nested.path);
                try writer.writeByte('#');
                try writer.writeAll(nested.sub_path);
            },
        }
    }

    pub fn toString(self: AssetPath, out_buf: []u8) ![]u8 {
        return self.toStringSep(out_buf, '#');
    }

    pub fn toStringSep(self: AssetPath, out_buf: []u8, sep: u8) ![]u8 {
        if (out_buf.len < self.strLen()) {
            return error.BufferTooSmall;
        }

        switch (self) {
            .simple => |path| {
                @memcpy(out_buf[0..path.len], path);
                return out_buf[0..path.len];
            },
            .nested => |nested| {
                @memcpy(out_buf[0..nested.path.len], nested.path);
                out_buf[nested.path.len] = sep;
                @memcpy(out_buf[nested.path.len + 1 .. nested.path.len + 1 + nested.sub_path.len], nested.sub_path);

                return out_buf[0..self.strLen()];
            },
        }
    }

    pub fn toStringAlloc(self: AssetPath, allocator: std.mem.Allocator) ![]u8 {
        const buf = try allocator.alloc(u8, self.strLen());
        return try self.toString(buf);
    }

    pub fn fromString(str: []const u8) AssetPath {
        if (std.mem.lastIndexOf(u8, str, "#")) |sep_idx| {
            return .{
                .nested = .{
                    .path = str[0..sep_idx],
                    .sub_path = str[sep_idx + 1 ..],
                },
            };
        }

        return .{ .simple = str };
    }
};
