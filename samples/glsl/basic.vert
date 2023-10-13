#version 330

#include "common.glsl"
#include "empty.glsl"

layout(location = 0) in vec3 position;

in Inputs {
    vec2 tex_coords;
} inputs;

out Outputs {
    vec4 frag_pos;
    vec2 tex_coords;
} outputs;

void main() {
    const vec4 color = vec4(0.3, 0.5 + 3, -3, 1);

    Rectangle[4] rect = Rectangle(vec2(1, 2), vec2(3, 4));
    Light light;

    for (int i = 0; i < len; i++) {
        print(i);
    }

    if (true) {
        return 123;
    } else {
        return 8;
    }
}

struct Light {
    vec3 color; // color of the light
    float size; // radius of the light
};
