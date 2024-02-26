pub const AssetId = u64;

pub const Handle = struct {
    pub const Scene = extern struct { id: AssetId = 0 };
    pub const Shader = extern struct { id: AssetId = 0 };
    pub const ShaderProgram = extern struct { id: AssetId = 0 };
    pub const Mesh = extern struct { id: AssetId = 0 };
    pub const Texture = extern struct { id: AssetId = 0 };
    pub const Material = extern struct { id: AssetId = 0 };
};
