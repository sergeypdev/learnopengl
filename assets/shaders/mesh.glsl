#extension GL_ARB_bindless_texture : enable
// Keep in sync with cpu
#define MAX_POINT_LIGHTS 8
#define PI 3.1415926535897932384626433832795

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

layout(location = 9) uniform vec3 emission;
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

struct Material {
  vec3 albedo;
  bool metallic;
  float roughness;
  vec3 emission;
};

Material evalMaterial() {
  Material result;
  result.albedo = textureSize(albedo_map, 0) == ivec2(0) ? pow(color, vec3(2.2)) : texture(albedo_map, VertexOut.uv).rgb;
  float fMetallic = textureSize(metallic_map, 0) == ivec2(0) ? metallic : texture(metallic_map, VertexOut.uv).r;
  result.metallic = fMetallic > 0.5;
  result.roughness = max(0.01, textureSize(roughness_map, 0) == ivec2(0) ? roughness : texture(roughness_map, VertexOut.uv).r);
  result.emission = textureSize(emission_map, 0) == ivec2(0) ? emission : texture(emission_map, VertexOut.uv).rgb;

  return result;
}

vec3 schlickFresnel(Material mat, float LDotH) {
  vec3 f0 = vec3(0.04); // dielectric
  if (mat.metallic) {
    f0 = mat.albedo;
  }

  return f0 + (1 - f0) * pow(1.0 - LDotH, 5);
}

float geomSmith(Material mat, float DotVal) {
  float k = (mat.roughness + 1.0) * (mat.roughness + 1.0) / 8.0;
  float denom = DotVal * (1 - k) + k;
  return 1.0 / denom;
}

float ggxDistribution(Material mat, float NDotH) {
  float alpha2 = mat.roughness * mat.roughness * mat.roughness * mat.roughness;
  float d = (NDotH * NDotH) * (alpha2 - 1) + 1;
  return alpha2 / (PI * d * d);
}

float lightAttenuation(float dist, float radius) {
  float d = max(dist - radius, 0);

  float denom = d/radius + 1;
  float att = 1 / (denom * denom);
  // TODO: cutoff
  att = max(att, 0);

  return att;
}


vec3 microfacetModel(Material mat, Light light, vec3 P, vec3 N) {
  vec3 diffuseBrdf = vec3(0); // metallic
  if (!mat.metallic) {
    diffuseBrdf = mat.albedo;
  }

  vec3 lightI = light.color.rgb * light.color.a;
  float lightRadius = light.vPos.w;
  vec3 L = light.vPos.xyz - P;
  float dist = length(L);
  L /= dist;

  float att = lightAttenuation(dist, lightRadius);
  lightI *= att;

  vec3 V = normalize(-P);
  vec3 H = normalize(V + L);

  float NDotH = dot(N, H);
  float LDotH = dot(L, H);
  float NDotL = max(dot(N, L), 0);
  float NDotV = dot(N, V);

  vec3 specBrdf = 0.25 * ggxDistribution(mat, NDotH) * schlickFresnel(mat, LDotH) * geomSmith(mat, NDotL) * geomSmith(mat, NDotV);

  return (diffuseBrdf + PI * specBrdf) * lightI * NDotL;
}

void main() {
  Material material = evalMaterial();
  
  vec3 N = textureSize(normal_map, 0) == ivec2(0) ? vec3(0.5) : vec3(texture(normal_map, VertexOut.uv).xy, 0);
  N = N * 2.0 - 1.0;
  N.z = sqrt(clamp(1 - N.x * N.x - N.y * N.y, 0, 1));
  N = normalize(N);
  N = normalize(VertexOut.vTBN * N);

  vec3 finalColor = vec3(0);

  for (int i = 0; i < lights_count; i++) {
    finalColor += microfacetModel(material, lights[i], VertexOut.vPos, N);
  }

  // finalColor += microfacetModel(material, Light(vec4(VertexOut.vPos + N, 0.01), vec4(1, 1, 1, 10)), VertexOut.vPos, N);

  FragColor = vec4(finalColor, 1.0f);

  float gamma = 2.2;
  FragColor.rgb = pow(FragColor.rgb, vec3(1.0/gamma));
}


#endif // FRAGMNET_SHADER
