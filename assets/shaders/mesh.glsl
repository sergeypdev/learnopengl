#extension GL_ARB_bindless_texture : enable
// Keep in sync with cpu
#define MAX_POINT_LIGHTS 8

// Types
struct Light {
  vec4 vPos;
  vec4 color;
};

// UBOs
layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

layout(std140, binding = 1) uniform Lights {
  Light lights[MAX_POINT_LIGHTS];
  uint lights_count;
};

// Uniforms
layout(location = 1) uniform mat4 model;

layout(location = 2) uniform vec3 color;
layout(location = 3, bindless_sampler) uniform sampler2D albedo_map;

layout(location = 4, bindless_sampler) uniform sampler2D normal_map;

layout(location = 5) uniform float metallic;
layout(location = 6, bindless_sampler) uniform sampler2D metallic_map;

layout(location = 7) uniform float roughness;
layout(location = 8, bindless_sampler) uniform sampler2D roughness_map;

layout(location = 9) uniform float emission;
layout(location = 10, bindless_sampler) uniform sampler2D emission_map;


// Input, output blocks

VERTEX_EXPORT VertexData {
  vec3 vPos;
  vec2 uv;
  mat3 vTBN;
} VertexOut;

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;
layout(location = 1) in vec3 aNormal;
layout(location = 2) in vec2 aUV;
layout(location = 3) in vec3 aTangent;

void main() {
    vec4 vPos = view * model * vec4(aPos.xyz, 1.0);
    gl_Position = projection * vPos;

    VertexOut.vPos = vPos.xyz / vPos.w; // I don't think this is needed, but leaving just in case
    VertexOut.uv = aUV;
    vec3 aBitangent = cross(aTangent, aNormal);
    vec3 T = normalize(vec3(view * model * vec4(aTangent, 0.0)));
    vec3 B = normalize(vec3(view * model * vec4(aBitangent, 0.0)));
    vec3 N = normalize(vec3(view * model * vec4(aNormal, 0.0)));
    VertexOut.vTBN = mat3(T, B, N);
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

out vec4 FragColor;

void main() {
  vec3 albedoColor = textureSize(albedo_map, 0) == ivec2(0) ? color : texture(albedo_map, VertexOut.uv).rgb;

  vec3 N = textureSize(normal_map, 0) == ivec2(0) ? vec3(0.5) : vec3(texture(normal_map, VertexOut.uv).xy, 0);
  N = N * 2.0 - 1.0;
  N.z = sqrt(clamp(1 - N.x * N.x - N.y * N.y, 0, 1));
  N = normalize(N);
  N = normalize(VertexOut.vTBN * N);

  vec3 finalColor = vec3(0);

  for (int i = 0; i < lights_count; i++) {
    float radius = lights[i].vPos.w;
    vec3 L = lights[i].vPos.xyz - VertexOut.vPos;
    float dist = length(L);
    float d = max(dist - radius, 0);
    L /= dist;

    float denom = d/radius + 1;
    float att = 1 / (denom * denom);
    // TODO: cutoff
    att = max(att, 0);

    float ndotl = max(dot(L, N), 0);

    finalColor += ndotl * lights[i].color.w * lights[i].color.xyz * att * albedoColor;
  }

  FragColor = vec4(finalColor, 1.0f);

  float gamma = 2.2;
  FragColor.rgb = pow(FragColor.rgb, vec3(1.0/gamma));
}

#endif // FRAGMNET_SHADER
