#version 330

#include "common.glsl"

void print();

int add(int x, int y) {
    return x + y;
}

int main() {
    const vec4 color = vec4(0.3, 0.5 + 3, -3, 1);

    const int len = 10;

    Rectangle r = Rectangle(vec2(1, 2), vec2(3, 4));

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
    vec4 color; // color of the light
    float size; // radius of the light
};
