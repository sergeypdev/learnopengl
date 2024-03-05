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

layout(location = 0) out vec3 downsample;

const float invGamma = 1.0 / 2.2;
vec3 toSRGB(vec3 v) { return pow(v, vec3(invGamma)); }

float RGBToLuminance(vec3 col)
{
    return dot(col, vec3(0.2126f, 0.7152f, 0.0722f));
}

float karisAverage(vec3 col)
{
    // Formula is 1 / (1 + luma)
    float luma = RGBToLuminance(toSRGB(col)) * 0.25f;
    return 1.0f / (1.0f + luma);
}

void main() {
  vec2 uv = VertexOut.uv;

  // Take 13 samples around current texel:
  // a - b - c
  // - j - k -
  // d - e - f
  // - l - m -
  // g - h - i
  // === ('e' is the current texel) ===

  vec3 a = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-2, 2)).rgb;
  vec3 b = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0, 2)).rgb;
  vec3 c = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 2, 2)).rgb;

  vec3 d = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-2, 0)).rgb;
  vec3 e = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0, 0)).rgb;
  vec3 f = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 2, 0)).rgb;

  vec3 g = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-2,-2)).rgb;
  vec3 h = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 0,-2)).rgb;
  vec3 i = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 2,-2)).rgb;

  vec3 j = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-1, 1)).rgb;
  vec3 k = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 1, 1)).rgb;
  vec3 l = textureLodOffset(src_sampler, uv, src_mip_level, ivec2(-1,-1)).rgb;
  vec3 m = textureLodOffset(src_sampler, uv, src_mip_level, ivec2( 1,-1)).rgb;

  if (src_mip_level == 0) {
    // Use Karis average to get rid of fireflies
    
    vec3 group1 = (a + b + d + e) * (0.125 / 4);
    vec3 group2 = (b + c + e + f) * (0.125 / 4);
    vec3 group3 = (d + e + g + h) * (0.125 / 4);
    vec3 group4 = (e + f + h + i) * (0.125 / 4);
    vec3 group5 = (j + k + l + m) * (0.5 / 4);
    group1 *= karisAverage(group1);
    group2 *= karisAverage(group2);
    group3 *= karisAverage(group3);
    group4 *= karisAverage(group4);
    group5 *= karisAverage(group5);
    downsample = group1 + group2 + group3 + group4 + group5;
  } else {
    // Apply weighted distribution:
    // 0.5 + 0.125 + 0.125 + 0.125 + 0.125 = 1
    // a,b,d,e * 0.125
    // b,c,e,f * 0.125
    // d,e,g,h * 0.125
    // e,f,h,i * 0.125
    // j,k,l,m * 0.5
    // This shows 5 square areas that are being sampled. But some of them overlap,
    // so to have an energy preserving downsample we need to make some adjustments.
    // The weights are the distributed, so that the sum of j,k,l,m (e.g.)
    // contribute 0.5 to the final color output. The code below is written
    // to effectively yield this sum. We get:
    // 0.125*5 + 0.03125*4 + 0.0625*4 = 1
    downsample = e*0.125;
    downsample += (a+c+g+i)*0.03125;
    downsample += (b+d+f+h)*0.0625;
    downsample += (j+k+l+m)*0.125;
  }
  downsample = max(downsample, 0.001f);
}


#endif // FRAGMNET_SHADER
