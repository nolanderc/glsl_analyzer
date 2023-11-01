#version 430 core


uniform layout(rgba8, binding = 0) image2D image2d_0;
uniform layout(rgba16i, binding = 1) iimageCubeArray iimagecubearray_1;
uniform layout(rgba16_snorm, binding = 2) restrict writeonly coherent image2DMSArray image2dmsarray_2;

uniform sampler2D sampler2d_0;
uniform isamplerCubeArray isamplercubearray_1;
uniform sampler2DMSArray sampler2dmsarray_2;


void main() {
    // <uniform layout(rgba8, binding = 0) image2D>
    image2d_0;
    // <uniform layout(rgba16i, binding = 1) iimageCubeArray>
    iimagecubearray_1;
    // <uniform layout(rgba16_snorm, binding = 2) restrict writeonly coherent image2DMSArray>
    image2dmsarray_2;

    // <unifrom sampler2D>
    sampler2d_0;
    // <uniform isamplerCubeArray>
    isamplercubearray_1;
    // <uniform sampler2DMSArray>
    sampler2dmsarray_2;

}

