import pathlib

from testing_utils import (
    ExpectFail,
    FileToTest,
    TokenKind
)




base_directory = pathlib.Path(__file__).parent.resolve()



files = [
    # TODO: Arrays, UBOs and SSBOs, images/samplers.
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
            (30,  5, "int (inout int, out vec4, in float, const float)", TokenKind.Function),
            # Redeclaration of the same signature should not produce duplicate info.
            (33,  5, "int (int)", TokenKind.Function),
            # Redeclaration with implicit `in` or `const` qualifier should not produce extra overloads.
  ExpectFail(36,  5, "int (int)", TokenKind.Function, \
                reason="Current version uses simple string comparison for removing overloads"),
            # Functions from visible overload set are present.
            (39,  5, {"int (int)", "uint (uint)"}, TokenKind.Function),
            # Overloads inaccessble in the current context should not be visible.
  ExpectFail(42,  5, "int (int)", TokenKind.Function, \
                reason="Bug. Should only show overloads declared previously"),
        ),
        completion_test_args=(
            # Basic function identifier presence.
            (30,  8, ("fun",), TokenKind.Function),
            # No duplicate completion entries on redeclaration.
            (33,  8, ("fun_redeclared",), TokenKind.Function),
            # Redeclaration with implicit `in` or `const` qualifier should not produce extra overloads.
  ExpectFail(36,  8, ("fun_redeclared_but_with_implicit_in",), TokenKind.Function, \
                reason="Current version does not remove duplicate overloads from the completion list"),
            # Overloads do not generate duplicate entries in the completion list.
  ExpectFail(39,  8, ("fun_overloaded",), TokenKind.Function, \
                reason="Current version does not remove duplicate overloads from the completion list"),
            # Inaccessible overload should not be visible.
  ExpectFail(42,  8, ("fun_overloaded_but_later",), TokenKind.Function, \
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
]

