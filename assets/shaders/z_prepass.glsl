
struct DrawCmdData {
  mat4 transform;
};

// UBOs
layout(std140, binding = 0) uniform Matrices {
  mat4 projection;
  mat4 view;
};

layout(std430, binding = 3) readonly buffer DrawCmdDatas {
  uint draws_count;
  // Access by gl_DrawID
  DrawCmdData draw_data[];
};

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
    mat4 model = draw_data[gl_DrawID].transform;
    mat4 viewModel = view * model;
    vec4 vPos = viewModel * vec4(aPos.xyz, 1.0);
    gl_Position = projection * vPos;
}

#endif

#if FRAGMENT_SHADER

void main() {}

#endif
