#version 430 core

// int
// exp\
vec4  \
float \
exp   \
// exp\

/*
int foo(vec3);

/*
struct A {
    int f1;
} a;

*/

struct A {
    float f1;
} a;

void main() {
    a;    // <struct A { float f1; }>
    A a;  // <struct A { float f1; }> <A>
    a.f1; // a.<float>
    /*
    foo(vec3(1.0));
    */
}
