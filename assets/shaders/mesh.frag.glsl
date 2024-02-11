#version 450 core

#define MAX_POINT_LIGHTS 1

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;

struct Light {
  vec4 pos;
  // vec4 color;
};

layout(binding = 1, std140) uniform Lights {
  Light lights[MAX_POINT_LIGHTS];
};

out vec4 FragColor;

void main() {
  vec3 diffuseColor = vec3(0.2, 0.5, 0.2);
  vec3 finalColor = vec3(0);

  for (int i = 0; i < MAX_POINT_LIGHTS; i++) {
    vec3 lightVec = normalize(lights[i].pos.xyz - position);
    float ndotl = dot(normal, lightVec);

    finalColor += ndotl * diffuseColor;
  }

  FragColor = vec4(finalColor, 1.0f);
}
