#version 430 core

layout (triangles) in;
layout (triangle_strip, max_vertices = 36) out;

in gl_PerVertex {
    vec4  gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
} gl_in[];

out gl_PerVertex {
    vec4  gl_Position;
    float gl_PointSize;
    float gl_ClipDistance[];
};


struct Data {
    int id;
};

in Interface {
    Data data;
    vec2 tex_coords;
} in_[];

out Interface {
    vec4 frag_pos;
    vec3 tex_coords; // vec3 for extra confusion
} out_;


struct Out {
    vec4 frag_pos;
    vec2 tex_coords;
};

out vec2 tex_coords;

void main() {
    for (int vert_id = 0; vert_id < 3; ++vert_id) {

        // <out Interface { vec4 frag_pos; vec3 tex_coords; }> . <vec3>
        out_.tex_coords =
            // <in Interface { Data data; vec2 tex_coords; }[]> . <vec2>
            vec3(in_[vert_id].tex_coords, 0.0);

        // data: Data, id: int
        if (vert_id == in_[vert_id].data.id) {}

        // frag_pos: vec4
        out_.frag_pos =
            // gl_Position: vec4
            gl_in[vert_id].gl_Position;


        // Some shadowing tests
        {
            // <out vec2>
            tex_coords =
                // <out Interface { vec4 frag_pos; vec3 tex_coords; }> . <vec3>
                out_.tex_coords.st;

            // tex_coords: vec3
            // Out <Out>
            Out out_ =
                // Out(<out Interface { ... }>, <out Interface { ... }>.st)
                Out(out_.frag_pos, out_.tex_coords.st);

            // gl_Position: vec4
            gl_Position =
                out_.frag_pos;

            // <out vec2> = <Out> . <vec2>
            tex_coords = out_.tex_coords;
        }


        EmitVertex();
    }
}
