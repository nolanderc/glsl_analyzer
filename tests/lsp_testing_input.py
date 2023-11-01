import pathlib

from testing_utils import (
    ExpectFail,
    FileToTest,
)

import expected_hover
import expected_completion


base_directory = pathlib.Path(__file__).parent.resolve()


files_to_test = [
    FileToTest(
        path="glsl-samples/well-formed/basic.vert",
        hover_test_args=(
            (18, 19, "const vec4"),
            (20, 20, "Rectangle[4]"),
            (24, 13, "void (int)"),
            (35, 13, "vec3"),
        )
    ),
    FileToTest(
        path="hover-and-completion/shadowing.frag",
        hover_test_args=(
            # Globals.
            ( 4, 11, "int"),
            ( 5, 11, "const int"),
            ( 8,  5, "int"),
            ( 9,  5, "const int"),
            # Shadowed by locals.
            (13, 23, "const int"),
            (13, 29, "const int"),
            # Self-shadowing (same name, different type).
            (15, 13, "int"),
            (15, 20, "const int"),
            # Correctly recover scope.
            (17, 13, "int"),
            (18, 13, "int"),
            (21,  5, "const int"),
            (22,  5, "int"),
            # Swap types in scope.
            (25, 15, "int"),
            (25, 22, "int"),
            (26, 15, "const int"),
            (26, 22, "int"),
            (28,  5, "const int"),
            (29,  5, "int"),
            # Shadowing by function parameter names.
            (35,  5, "const int"),
            (36,  5, "int"),
        )
    ),
    FileToTest(
        path="hover-and-completion/struct_fields.frag",
        hover_test_args=(
            # Globals.
            (30,  5, "const AAA"),
            (31,  5, "const BBB"),
            (32,  5, "const CCC"),
            # Fields of structs.
            (35,  9, "int"),
            (36,  9, "float"),
            (37,  9, "AAA"),
            # Fields of fields of structs.
            (40, 13, "int"),
            (41, 13, "float"),
            (42, 13, "uint"),
            (43, 13, "float"),
            (44, 13, "int"),
            (45, 13, "bool"),
        ),
        completion_test_args=(
            # Correctly list fields of a struct.
            (35,  9, ("f1", "f2", "fa")),
            (36,  9, ("f1", "f2", "fb")),
            (37,  9, ("f1", "f2", "aaa", "bbb")),
            # Correctly list fields of a field of a struct.
            (40, 13, ("f1", "f2", "fa")),
            (43, 13, ("f1", "f2", "fb")),
        )
    ),
    FileToTest(
        path="hover-and-completion/interface_blocks.geom",
        hover_test_args=(
            # Correct named interface types and their fields.
            (45,  9, "out Interface { vec4 frag_pos; vec3 tex_coords; }"),
            (45, 14, "vec3"),
            (47, 18, "in Interface { Data data; vec2 tex_coords; }[]"),
            (47, 31, "vec2"),
            # Field of a field of a named interface block.
            (50, 37, "Data"),
            (50, 42, "int"),
            # Correct type of explicitly declared input params.
            (53, 14, "vec4"),
            (55, 28, "vec4"),
            # Check for no confusion between the same names.
            (61, 13, "out vec2"),
            (63, 17, "out Interface { vec4 frag_pos; vec3 tex_coords; }"),
            (63, 22, "vec3"),
            # Shadowing an interface with a similar struct.
            (67, 17, "Out"),
            (69, 21, "out Interface { vec4 frag_pos; vec3 tex_coords; }"),
            # Possible confusion on type of tex_coords between `struct Out` and `out Interface`.
            (69, 41, "vec3"),
            # Correct type of explicitly declared output params.
            (72, 13, "vec4"),
            # More possible name confusion on `tex_coords`.
            (76, 13, "out vec2"),
            (76, 26, "Out"),
            (76, 31, "vec2"),
        ),
        completion_test_args=(
            # Fields of the interface blocks are resolved correctly.
            (45, 14, ("frag_pos", "tex_coords")),
            (47, 31, ("data", "tex_coords")),
            # Field of a fild of a named interface block.
            (50, 42, ("id",)),
            # Fields of the redeclared default input block.
            (55, 28, ("gl_ClipDistance", "gl_Position", "gl_PointSize")),
            # Primitive types have no fields.
  ExpectFail(63, 33, None, \
                reason="Undesirable behavior. Although works correctly for blocks and structs."),
        ),
    ),
    FileToTest(
        path="hover-and-completion/functions.frag",
        hover_test_args=(
            # Basic function declaration with qualified argument types.
            (30,  5, expected_hover.FunctionIdent("int (inout int, out vec4, in float, const float)")),
            # Redeclaration of the same signature should not produce duplicate info.
            (33,  5, expected_hover.FunctionIdent("int (int)")),
            # Redeclaration with implicit `in` or `const` qualifier should not produce extra overloads.
  ExpectFail(36,  5, expected_hover.FunctionIdent("int (int)"), \
                reason="Current version uses simple string comparison for removing overloads"),
            # Functions from visible overload set are present.
            (39,  5, expected_hover.FunctionIdent({"int (int)", "uint (uint)"})),
            # Overloads inaccessble in the current context should not be visible.
  ExpectFail(42,  5, expected_hover.FunctionIdent("int (int)"), \
                reason="Bug. Should only show overloads declared previously"),
        ),
        completion_test_args=(
            # Basic function identifier presence.
            (30,  8, ("fun",)),
            # No duplicate completion entries on redeclaration.
            (33,  8, ("fun_redeclared",)),
            # Redeclaration with implicit `in` or `const` qualifier should not produce extra overloads.
  ExpectFail(36,  8, ("fun_redeclared_but_with_implicit_in",), \
                reason="Current version does not remove duplicate overloads from the completion list"),
            # Overloads do not generate duplicate entries in the completion list.
  ExpectFail(39,  8, ("fun_overloaded",), \
                reason="Current version does not remove duplicate overloads from the completion list"),
            # Inaccessible overload should not be visible.
  ExpectFail(42,  8, ("fun_overloaded_but_later",), \
                reason="Bug. Inaccessible overloads are visible in completion")
        ),
    ),
    FileToTest(
        path="hover-and-completion/comments.frag",
        hover_test_args=(
            # Hover in comments should return nothing.
            ( 3,  4, None),
            ( 4,  4, None),
            ( 5,  1, None),
            ( 6,  1, None),
            ( 7,  1, None),
            ( 8,  4, None),
            (11,  8, None),
            (16,  3, None),
            # Commented out definitions should not confuse hover info.
            (25,  5, "struct A { float f1; }"),
            (26,  5, "struct A { float f1; }"),
            (26,  7, "A"),
            (27,  7, "float"),
            (29,  5, None),
        ),
        completion_test_args=(
            # Completion in comments should be disabled.
            ( 3,  7, None),
            ( 4,  7, None),
            ( 5,  5, None),
            ( 6,  6, None),
            ( 7,  4, None),
            ( 8,  7, None),
            (29,  8, None),
        )
    ),
    FileToTest(
        path="hover-and-completion/hover_positions.frag",
        hover_test_args=(
            # Past-the-end hover should only trigger when cursor is adjacent to the token.
            ( 9, 10, "int"),
            ( 9, 11, None),
            # Past-the-end break by space and newline.
            (11, 11, None),
            (13,  1, None),
            # Past-the-end break by comments.
            (15, 17, None),
            (17,  1, None),
            # Nearby identifiers.
            (19, 10, "int"),
            (19, 11, "const int"),
        )
    ),
    FileToTest(
        path="hover-and-completion/static_arrays.frag",
        hover_test_args=(
            # Shadowing.
            (18,  5, "const int[3]"),
            (20,  5, "const int[]"),
            # Implicit size.
            (22,  5, "int[]"),
            # Nested arrays.
            (24,  5, "int[2][3]"),
            # Nested with partially implicit size.
            (26,  5, "const int[][3]"),
            # Arrays of structs.
            (32,  5, "const Elem[3]"),
            (38,  5, "const Elem[3]"),
            (38, 17, "int"),
        ),
        completion_test_args=(
            # Arrays of structs.
  ExpectFail(36, 14, None, \
                reason="Bug. Arrays have no fields."),
            (38, 17, ("field1",)),
        ),
    ),
    FileToTest(
        path="hover-and-completion/buffers.frag",
        hover_test_args=(
            # Global buffer fields.
            (60,  5, "mat4"),
            (61,  5, "mat3[]"),
            # Scoped buffer fields.
            (63,  5, "layout(std140, binding = 1) uniform UBuffer1 { vec4 field0; }"),
            (63, 14, "vec4"),
            (65,  5, "layout(std430, binding = 1) buffer SSBuffer1 { mat2 field1[]; }"),
            (65, 15, "mat2[]"),
            (67,  5, "layout(std430, binding = 2) buffer SSBuffer2 { vec3 field0[]; }"),
            (67, 15, "vec3[]"),
            # Nested fields.
            (69,  5, "layout(std430, binding = 3) buffer SSBuffer3 { AAA aaa[][3]; }"),
            (69, 15, "AAA[][3]"),
            (69, 25, "int"),
            # Nested structs with repeating names.
            (71,  5, "layout(std430, binding = 4) buffer SSBuffer4 { CCC ssbuffer4; }"),
            (71, 15, "CCC"),
            (71, 25, "BBB"),
            (71, 35, "uint"),
            # Array of uniform buffers.
            (73,  5, "layout(std140, binding = 2) uniform UBuffer2 { mat2 field0; }[3]"),
            (73, 17, "mat2"),
            # Qualifiers.
            (75,  5, "layout(std430, binding = 5) restrict writeonly coherent buffer SSBuffer5 { bool field0; }"),
            (75, 15, "bool"),
        ),
        completion_test_args=(
            # Nested structs with repeating names.
            (71, 15, ("ssbuffer4",)),
            (71, 25, ("ssbuffer4",)),
            (71, 35, ("ssbuffer4",)),
        ),
    ),
    FileToTest(
        path="hover-and-completion/images_and_samplers.frag",
        hover_test_args=(
            (15,  5, "uniform layout(rgba8, binding = 0) image2D"),
            (17,  5, "uniform layout(rgba16i, binding = 1) iimageCubeArray"),
            (19,  5, "uniform layout(rgba16_snorm, binding = 2) restrict writeonly coherent image2DMSArray"),
            (22,  5, "uniform sampler2D"),
            (24,  5, "uniform isamplerCubeArray"),
            (26,  5, "uniform sampler2DMSArray"),
        ),
    ),
]

