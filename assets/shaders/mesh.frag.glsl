#version 450 core

// Keep in sync with cpu
#define MAX_POINT_LIGHTS 8

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;

struct Light {
  vec4 pos;
  vec4 color;
};

layout(binding = 1, std140) uniform Lights {
  Light lights[MAX_POINT_LIGHTS];
  uint lights_count;
};

layout(location = 2) uniform vec3 color;

out vec4 FragColor;

void main() {
  vec3 diffuseColor = color;
  vec3 finalColor = vec3(0);

  for (int i = 0; i < lights_count; i++) {
    float radius = lights[i].pos.w;
    vec3 L = lights[i].pos.xyz - position;
    float dist = length(L);
    float d = max(dist - radius, 0);
    L /= dist;

    float denom = d/radius + 1;
    float att = 1 / (denom * denom);
    // TODO: cutoff
    att = max(att, 0);

    float ndotl = max(dot(L, normal), 0);

    finalColor += ndotl * lights[i].color.xyz * att * diffuseColor;
  }

  FragColor = vec4(finalColor, 1.0f);
}
