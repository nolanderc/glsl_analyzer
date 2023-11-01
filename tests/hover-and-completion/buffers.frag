#version 430 core


// Simple name resolution for global and local scopes.
layout(std140, binding = 0) uniform UBuffer0 {
    mat4 field0;
};

layout(std140, binding = 1) uniform UBuffer1 {
    vec4 field0;
} ubuffer1;

layout(std430, binding = 0) buffer SSBuffer0 {
    mat3 field1[];
};

layout(std430, binding = 1) buffer SSBuffer1 {
    mat2 field1[];
} ssbuffer1;

layout(std430, binding = 2) buffer SSBuffer2 {
    vec3 field0[];
} ssbuffer2;

// Nested aggregates.
struct AAA {
    int field0;
};

layout(std430, binding = 3) buffer SSBuffer3 {
    AAA aaa[][3];
} ssbuffer3;

// Nested with repeating names.
struct BBB {
    uint ssbuffer4;
};

struct CCC {
    BBB ssbuffer4;
};

layout(std430, binding = 4) buffer SSBuffer4 {
    CCC ssbuffer4;
} ssbuffer4;

// Arrays of uniform buffers.
layout(std140, binding = 2) uniform UBuffer2 {
    mat2 field0;
} ubuffer2[3];

// Qualifiers.
layout(std430, binding = 5) restrict writeonly coherent buffer SSBuffer5 {
    bool field0;
} ssbuffer5;


void main() {

    field0; // <mat4>
    field1; // <mat3[]>
    // <layout(std140, binding = 1) uniform UBuffer1 { vec4 field0; }> . <vec4>
    ubuffer1.field0;
    // <layout(std430, binding = 1) buffer SSBuffer1 { mat2 field1[]; }> . <mat2[]>
    ssbuffer1.field1;
    // <layout(std430, binding = 2) buffer SSBuffer2 { vec3 field0[]; }> . <vec3[]>
    ssbuffer2.field0;
    // <layout(std430, binding = 3) buffer SSBuffer3 { AAA aaa[][3]; }> . <AAA[][3]> . <int>
    ssbuffer3.aaa[0][0].field0;
    // <layout(std430, binding = 4) buffer SSBuffer4 { CCC ssbuffer4; }> . <CCC> . <BBB> . <uint>
    ssbuffer4.ssbuffer4.ssbuffer4.ssbuffer4;
    // <layout(std140, binding = 2) uniform UBuffer2 { mat2 field0; }[3]> . <mat2>
    ubuffer2[0].field0;
    // <layout(std430, binding = 5) restrict writeonly coherent buffer SSBuffer5 { bool field0; }> . <bool>
    ssbuffer5.field0;

}
