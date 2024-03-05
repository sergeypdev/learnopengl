// Input, output blocks
VERTEX_EXPORT VertexData {
  vec2 uv;
} VertexOut;

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
  gl_Position = vec4(aPos, 1);
  VertexOut.uv = aPos.xy * 0.5 + 0.5;
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER


layout(binding = 0) uniform sampler2D src_sampler;
layout(location = 19) uniform int src_mip_level;
layout(location = 20) uniform float bloom_strength = 1;

layout(location = 0) out vec3 upsample;

void main() {
  vec2 uv = VertexOut.uv;

  // Take 9 samples around current texel:
  // a - b - c
  // d - e - f
  // g - h - i
  // === ('e' is the current texel) ===
  vec3 a = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-1, 1)).rgb;
  vec3 b = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0, 1)).rgb;
  vec3 c = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 1, 1)).rgb;

  vec3 d = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-1, 0)).rgb;
  vec3 e = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0, 0)).rgb;
  vec3 f = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 1, 0)).rgb;

  vec3 g = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-1,-1)).rgb;
  vec3 h = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0,-1)).rgb;
  vec3 i = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 1,-1)).rgb;

  // Apply weighted distribution, by using a 3x3 tent filter:
  //  1   | 1 2 1 |
  // -- * | 2 4 2 |
  // 16   | 1 2 1 |
  upsample = e*4.0;
  upsample += (b+d+f+h)*2.0;
  upsample += (a+c+g+i);
  upsample *= 1.0 / 16.0;
  upsample *= bloom_strength;
}


#endif // FRAGMNET_SHADER
