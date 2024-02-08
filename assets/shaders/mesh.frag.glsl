#version 450 core

layout(location = 0) in vec3 normal;

out vec4 FragColor;

void main() {
    FragColor = vec4(0.2, 0.5, 0.2, 1.0f);
}
