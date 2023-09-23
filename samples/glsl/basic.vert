#version 330

#include "thing.glsl"

int main() {
    const vec4 color = vec4(
        0.3, 0.5 + 3,
        -3, 1
    );
}

struct Light {
    vec4 color; // radius of the light
    float size;
};

