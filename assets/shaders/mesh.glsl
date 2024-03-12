#extension GL_ARB_bindless_texture : enable
// Keep in sync with cpu
#define MAX_POINT_LIGHTS 8
#define PI 3.1415926535897932384626433832795

// Types
struct Light {
  vec4 vPos;
  vec4 color;

  mat4 view_mat;

  // for directional lights contains view projection matrices for each split
  // TODO: compress this somehow
  mat4[4] view_proj_mats;

  // x, y = near, far for point lights
  // z = shadow map index
  // w = csm split count for directional lights
  vec4 params;
  vec4 csm_split_points; // TODO: Maybe increase to 8, though it's probably too many
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

int getShadowMapIndex(int lightIdx) {
  return int(lights[lightIdx].params.z);
}

int getCSMSplitCount(int lightIdx) {
  return clamp(int(lights[lightIdx].params.w), 0, 4);
}

int getCSMSplit(int lightIdx, float depth) {
  int totalSplits = getCSMSplitCount(lightIdx);

  for (int i = 0; i < totalSplits; i++) {
    if (depth > lights[lightIdx].csm_split_points[i]) {
      return i;
    }
  }

  return totalSplits - 1;
}

// Uniforms
layout(location = 1) uniform mat4 model;

layout(location = 2) uniform vec3 color;
layout(location = 3, bindless_sampler) uniform sampler2D albedo_map;
layout(location = 4) uniform vec2 albedo_map_uv_scale = vec2(1);

layout(location = 5, bindless_sampler) uniform sampler2D normal_map;
layout(location = 6) uniform vec2 normal_map_uv_scale = vec2(1);

layout(location = 7) uniform float metallic;
layout(location = 8, bindless_sampler) uniform sampler2D metallic_map;
layout(location = 9) uniform vec2 metallic_map_uv_scale = vec2(1);

layout(location = 10) uniform float roughness;
layout(location = 11, bindless_sampler) uniform sampler2D roughness_map;
layout(location = 12) uniform vec2 roughness_map_uv_scale = vec2(1);

layout(location = 13) uniform vec3 emission;
layout(location = 14, bindless_sampler) uniform sampler2D emission_map;
layout(location = 15) uniform vec2 emission_map_uv_scale = vec2(1);

layout(location = 16, bindless_sampler) uniform sampler2DArrayShadow shadow_maps;
layout(location = 17, bindless_sampler) uniform samplerCubeArrayShadow cube_shadow_maps;


// Input, output blocks

VERTEX_EXPORT VertexData {
  vec3 vPos;
  vec2 uv;
  mat3 vTBN;
  vec3 wPos;
  vec3 wNormal;
} VertexOut;

float random(vec4 seed4) {
  float dot_product = dot(seed4, vec4(12.9898,78.233,45.164,94.673));
  return fract(sin(dot_product) * 43758.5453);
}

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
    vec4 wPos = model * vec4(aPos.xyz, 1.0);
    VertexOut.wPos = wPos.xyz / wPos.w;
    VertexOut.wNormal = normalize(model * vec4(aNormal, 0.0)).xyz;
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

out vec4 FragColor;

struct Material {
  vec3 albedo;
  bool metallic;
  float roughness;
  vec3 emission;
};

Material evalMaterial() {
  Material result;
  result.albedo = textureSize(albedo_map, 0) == ivec2(0) ? pow(color, vec3(2.2)) : texture(albedo_map, VertexOut.uv * albedo_map_uv_scale).rgb;
  float fMetallic = textureSize(metallic_map, 0) == ivec2(0) ? metallic : texture(metallic_map, VertexOut.uv * metallic_map_uv_scale).b;
  result.metallic = fMetallic > 0.1;
  result.roughness = max(0.01, textureSize(roughness_map, 0) == ivec2(0) ? roughness : texture(roughness_map, VertexOut.uv * roughness_map_uv_scale).g);
  result.emission = textureSize(emission_map, 0) == ivec2(0) ? emission : texture(emission_map, VertexOut.uv * emission_map_uv_scale).rgb;

  return result;
}

vec3 schlickFresnel(Material mat, float LDotH) {
  vec3 f0 = vec3(0.04); // dielectric
  if (mat.metallic) {
    f0 = mat.albedo;
  }

  return f0 + (1 - f0) * pow(1.0 - LDotH, 5);
}

const float eps = 0.0000001;

float geomSmith(Material mat, float DotVal) {
  float k = (mat.roughness + 1.0) * (mat.roughness + 1.0) / 8.0;
  float denom = DotVal * (1 - k) + k;
  return 1.0 / max(denom, eps);
}

float ggxDistribution(Material mat, float NDotH) {
  float alpha2 = mat.roughness * mat.roughness * mat.roughness * mat.roughness;
  float d = (NDotH * NDotH) * (alpha2 - 1) + 1;
  return alpha2 / max((PI * d * d), eps);
}

float lightAttenuation(float point, float dist, float radius) {
  float d = max(dist - radius, 0) * point;

  float denom = d/radius + 1;
  float att = 1 / (denom * denom);
  // TODO: cutoff
  att = max(att, 0);

  return att;
}

vec2 poissonDisk[4] = vec2[](
  vec2( -0.94201624, -0.39906216 ),
  vec2( 0.94558609, -0.76890725 ),
  vec2( -0.094184101, -0.92938870 ),
  vec2( 0.34495938, 0.29387760 )
);

const vec3 csm_split_colors[4] = vec3[](
  vec3(0f, 1f, 0.17f),
  vec3(1f, 0.2f, 0.6f),
  vec3(0.17f, 0.78f, 1f),
  vec3(0.91f, 0.93f, 0.64f)
);

float map(float value, float min1, float max1, float min2, float max2) {
  return min2 + (value - min1) * (max2 - min2) / (max1 - min1);
}

vec3 microfacetModel(Material mat, int light_idx, Light light, vec3 P, vec3 N) {
  int csm_split_idx = getCSMSplit(light_idx, P.z);
  mat.albedo = mix(mat.albedo, csm_split_colors[csm_split_idx], 0.8);

  vec3 diffuseBrdf = vec3(0); // metallic
  if (!mat.metallic) {
    diffuseBrdf = mat.albedo;
  }

  // 0 - means directional, 1 - means point light
  float point = light.vPos.w;
  vec3 lightI = light.color.rgb;
  float lightRadius = max(light.color.a, eps);
  vec3 L = mix(-light.vPos.xyz, light.vPos.xyz - P, light.vPos.w);
  float dist = max(length(L), eps);
  L /= dist;

  // TODO: I think this is uniform control flow
  // so makes sense to use `if` there for directional/point
  // and don't calculate attenuation for directional at all
  float att = lightAttenuation(
    point,
    dist,
    lightRadius
  );
  lightI *= att;

  vec3 V = normalize(-P);
  vec3 H = normalize(V + L);

  float NDotH = dot(N, H);
  float LDotH = dot(L, H);
  float NDotL = max(dot(N, L), 0);
  float NDotV = dot(N, V);

  float normal_offset_scale = clamp(1 - NDotL, 0, 1);
  normal_offset_scale *= 10; // constant

  int shadow_map_idx = getShadowMapIndex(light_idx);
  shadow_map_idx = 0;

  float constant_bias = 0.003;
  float shadow_mult = 1;
  vec4 shadow_offset = vec4(VertexOut.wNormal * normal_offset_scale, 0);
  if (point == 1) {
    vec2 shadow_map_texel_size = 1.0 / vec2(textureSize(cube_shadow_maps, 0));
    shadow_offset *= shadow_map_texel_size.x;
    vec3 shadow_dir = (light.view_mat * vec4(VertexOut.wPos, 1.0)).xyz;
    float world_depth = length(shadow_dir.xyz);
    shadow_dir = normalize((light.view_mat * (vec4(VertexOut.wPos, 1.0) + shadow_offset)).xyz);
    float mapped_depth = map(world_depth, light.params.x, light.params.y, 0, 1);

    vec4 texcoord;
    texcoord.xyz = shadow_dir;
    texcoord.w = float(light_idx);

    float sum = 0;
    for (float z = -1; z <= 1; z += 1) {
      for (float y = -1; y <= 1; y += 1) {
        for (float x = -1; x <= 1; x += 1) {
          sum += texture(cube_shadow_maps, vec4(normalize(texcoord.xyz + vec3(x, y, z) * shadow_map_texel_size.x), texcoord.w), mapped_depth - constant_bias);
        }
      }
    }

    shadow_mult = sum / 27;
  } else {
    // Directional shadow
    vec2 shadow_map_texel_size = 1.0 / vec2(textureSize(shadow_maps, 0));
    shadow_offset *= shadow_map_texel_size.x;
    vec4 shadow_pos = light.view_proj_mats[csm_split_idx] * vec4(VertexOut.wPos, 1.0);
    shadow_pos.xy = (light.view_proj_mats[csm_split_idx] * (vec4(VertexOut.wPos, 1.0) + shadow_offset)).xy;
    shadow_pos /= shadow_pos.w;
    shadow_pos.xy = shadow_pos.xy * 0.5 + 0.5; // [-1, 1] to [0, 1]
    shadow_pos.z = min(shadow_pos.z, 1);
    shadow_pos.z -= constant_bias;

    vec4 texcoord;
    texcoord.xyw = shadow_pos.xyz; // sampler2DArrayShadow strange texcoord mapping
    texcoord.z = shadow_map_idx + csm_split_idx;

    float sum = 0;
    for (float y = -1.5; y <= 1.5; y += 1) {
      for (float x = -1.5; x <= 1.5; x += 1) {
        sum += texture(shadow_maps, vec4(texcoord.xy + vec2(x, y) * shadow_map_texel_size, texcoord.zw));
      }
    }

    shadow_mult = sum / 16.0;
  }
  shadow_mult = clamp(shadow_mult, 0, 1);

  vec3 specBrdf = 0.25 * ggxDistribution(mat, NDotH) * schlickFresnel(mat, LDotH) * geomSmith(mat, NDotL) * geomSmith(mat, NDotV);

  return (diffuseBrdf + PI * specBrdf) * lightI * NDotL * shadow_mult + mat.emission;
}

void main() {
  Material material = evalMaterial();
  
  vec3 N = textureSize(normal_map, 0) == ivec2(0) ? vec3(0.5) : vec3(texture(normal_map, VertexOut.uv * normal_map_uv_scale).xy, 0);
  N = N * 2.0 - 1.0;
  N.z = sqrt(clamp(1 - N.x * N.x - N.y * N.y, 0, 1));
  N = normalize(N);
  N = normalize(VertexOut.vTBN * N);

  vec3 finalColor = vec3(0);

  for (int i = 0; i < clamp(lights_count, uint(0), uint(MAX_POINT_LIGHTS)); i++) {
    finalColor += microfacetModel(material, i, lights[i], VertexOut.vPos, N);
  }

  FragColor = vec4(finalColor, 1.0f);
}


#endif // FRAGMNET_SHADER
