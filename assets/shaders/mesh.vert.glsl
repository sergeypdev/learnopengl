#version 450 core

layout(location = 0) in vec3 aPos;
layout(location = 1) in vec3 aNorm;

layout(location = 0) out vec3 position;
layout(location = 1) out vec3 normal;

layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

layout(location = 1) uniform mat4 model;

void main() {
    gl_Position = projection * view * model * vec4(aPos.xyz, 1.0);
    vec4 posWorld = model * vec4(aPos, 1.0);
    position = posWorld.xyz / posWorld.w;
    normal = aNorm;
}
