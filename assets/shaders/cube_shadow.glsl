// UBOs
layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

// Uniforms
layout(location = 1) uniform mat4 model;
layout(location = 18) uniform vec2 near_far;

// Input, output blocks
VERTEX_EXPORT VertexData {
  vec3 vPos;
} VertexOut;

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
  vec4 vPos = view * model * vec4(aPos.xyz, 1.0);
  gl_Position = projection * vPos;
  VertexOut.vPos = vPos.xyz;
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

float map(float value, float min1, float max1, float min2, float max2) {
  return min2 + (value - min1) * (max2 - min2) / (max1 - min1);
}

void main() {
  gl_FragDepth = map(length(VertexOut.vPos), near_far.x, near_far.y, 0, 1);
}


#endif // FRAGMNET_SHADER
