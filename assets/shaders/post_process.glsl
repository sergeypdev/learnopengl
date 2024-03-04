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

// Translated from https://github.com/TheRealMJP/BakingLab/blob/master/BakingLab/ACES.hlsl
// sRGB => XYZ => D65_2_D60 => AP1 => RRT_SAT
const mat3 ACESInputMat = mat3(
  vec3(0.59719, 0.07600, 0.02840),
  vec3(0.35458, 0.90834, 0.01566),
  vec3(0.04823, 0.13383, 0.83777)
);

// ODT_SAT => XYZ => D60_2_D65 => sRGB
const mat3 ACESOutputMat = mat3(
  vec3( 1.60475, -0.10208, -0.00327),
  vec3(-0.53108,  1.10813, -0.00605),
  vec3(-0.07367, -0.07276,  1.07602)
);

vec3 RRTAndODTFit(vec3 v) {
    vec3 a = v * (v + 0.0245786f) - 0.000090537f;
    vec3 b = v * (0.983729f * v + 0.4329510f) + 0.238081f;
    return a / b;
}

vec3 ACESFitted(vec3 color)
{
    color = ACESInputMat * color;

    // Apply RRT and ODT
    color = RRTAndODTFit(color);

    color = ACESOutputMat * color;

    // Clamp to [0, 1]
    color = clamp(color, 0, 1);

    return color;
}

layout(binding = 0) uniform sampler2D screen_sampler;

out vec4 FragColor;

vec3 linearToSRGB(vec3 color) {
  vec3 x = color * 12.92f;
  vec3 y = 1.055f * pow(clamp(color, 0, 1), vec3(1.0f / 2.4f)) - 0.055f;

  vec3 clr = color;
  clr.r = color.r < 0.0031308f ? x.r : y.r;
  clr.g = color.g < 0.0031308f ? x.g : y.g;
  clr.b = color.b < 0.0031308f ? x.b : y.b;

  return clr;
}

void main() {
  vec3 hdr_color = texture(screen_sampler, VertexOut.uv).rgb;
  hdr_color = ACESFitted(hdr_color);

  FragColor.rgb = linearToSRGB(hdr_color);
  FragColor.a = 1;
}


#endif // FRAGMNET_SHADER
