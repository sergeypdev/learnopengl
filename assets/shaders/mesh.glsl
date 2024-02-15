#extension GL_ARB_bindless_texture : enable
// Keep in sync with cpu
#define MAX_POINT_LIGHTS 8

// Types
struct Light {
  vec4 pos;
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
layout(location = 3, bindless_sampler) uniform sampler2D diffuse;

// Input, output blocks

VERTEX_EXPORT VertexData {
  vec3 position;
  vec3 normal;
  vec2 uv;
} VertexOut;

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;
layout(location = 1) in vec3 aNorm;
layout(location = 2) in vec2 aUV;

void main() {
    gl_Position = projection * view * model * vec4(aPos.xyz, 1.0);
    vec4 posWorld = model * vec4(aPos, 1.0);
    VertexOut.position = posWorld.xyz / posWorld.w;
    VertexOut.normal = aNorm;
    VertexOut.uv = aUV;
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

out vec4 FragColor;

void main() {
  vec3 diffuseColor = texture(diffuse, VertexOut.uv).rgb;
  vec3 finalColor = vec3(0);

  for (int i = 0; i < lights_count; i++) {
    float radius = lights[i].pos.w;
    vec3 L = lights[i].pos.xyz - VertexOut.position;
    float dist = length(L);
    float d = max(dist - radius, 0);
    L /= dist;

    float denom = d/radius + 1;
    float att = 1 / (denom * denom);
    // TODO: cutoff
    att = max(att, 0);

    float ndotl = max(dot(L, VertexOut.normal), 0);

    finalColor += ndotl * lights[i].color.w * lights[i].color.xyz * att * diffuseColor;
  }

  FragColor = vec4(finalColor, 1.0f);

  float gamma = 2.2;
  FragColor.rgb = pow(FragColor.rgb, vec3(1.0/gamma));
}

#endif // FRAGMNET_SHADER
