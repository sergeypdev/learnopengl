pub const AssetId = u32;

pub const Handle = struct {
    pub const Shader = extern struct { id: AssetId = 0 };
    pub const ShaderProgram = extern struct { id: AssetId = 0 };
    pub const Mesh = struct { id: AssetId = 0 };
};
