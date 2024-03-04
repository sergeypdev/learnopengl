// Input, output blocks
VERTEX_EXPORT VertexData {
  vec2 uv;
} VertexOut;

#if VERTEX_SHADER

layout(location = 0) in vec3 aPos;

void main() {
  gl_Position = vec4(aPos, 1);
  VertexOut.uv = aPos.xy * 0.5 + 0.5;
}
#endif // VERTEX_SHADER

#if FRAGMENT_SHADER

layout(binding = 0) uniform sampler2D screen_sampler;

out vec4 FragColor;

void main() {
  float gamma = 2.2;
  vec4 hdr_color = texture(screen_sampler, VertexOut.uv);

  FragColor.rgb = pow(hdr_color.rgb, vec3(1.0/gamma));
  FragColor.a = 1;
}


#endif // FRAGMNET_SHADER
