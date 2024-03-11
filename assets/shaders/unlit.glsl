#extension GL_ARB_bindless_texture : enable

// UBOs
layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

// Uniforms
layout(location = 1) uniform mat4 model;

layout(location = 2) uniform vec3 color;

// Input, output blocks

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
  gl_Position = projection * view * model * vec4(aPos.xyz, 1.0);
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

out vec4 FragColor;

void main() {
  FragColor = vec4(color, 1.0f);
}


#endif // FRAGMNET_SHADER
