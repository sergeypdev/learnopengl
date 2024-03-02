// UBOs
layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

// Uniforms
layout(location = 1) uniform mat4 model;

// Input, output blocks

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
    gl_Position = projection * view * model * vec4(aPos.xyz, 1.0);
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

void main() {
  gl_FragDepth = gl_FragCoord.z / gl_FragCoord.w;
}


#endif // FRAGMNET_SHADER
