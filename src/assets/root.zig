pub const AssetId = u64;

pub const Handle = struct {
    pub const Shader = extern struct { id: AssetId = 0 };
    pub const ShaderProgram = extern struct { id: AssetId = 0 };
    pub const Mesh = struct { id: AssetId = 0 };
    pub const Texture = struct { id: AssetId = 0 };
};
