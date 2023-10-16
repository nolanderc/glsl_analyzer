import os
import pathlib
import re

import lsprotocol.types as lspt

import pytest
import pytest_subtests
import pytest_lsp


base_directory = pathlib.Path(__file__).parent.resolve()



class ExpectFail:
    """A wrapper class that signals that a test with
    this particular set of arguments is expected to fail"""
    def __init__(self, args):
        self.args = args


def unwrap_args(args):
    """Removes top-level `ExpectFail` if present
    and returns a boolean indicating whether
    success is expected as a last tuple element"""
    if isinstance(args, ExpectFail):
        return (*args.args, False)
    else:
        return (*args, True)


class FileToTest:
    HoverExpectedType = str|None
    HoverTestArgs     = tuple[int, int, HoverExpectedType]

    def __init__(
        self,
        path: str,
        hover_test_args: tuple[HoverTestArgs|ExpectFail] = (),
    ):
        self.path = path
        self.hover_test_args = hover_test_args





files = [
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
        path="glsl-samples/hover/shadowing.frag",
        hover_test_args=(
            ( 4, 11, "int"),
            ( 5, 11, "const int"),
            ( 8,  5, "int"),
            ( 9,  5, "const int"),
            (13, 23, "const int"),
            (13, 29, "const int"),
            (15, 13, "int"),
            ExpectFail((15, 20, "const int")),
            (21,  5, "const int"),
            (22,  5, "int"),
            (25, 15, "int"),
            (25, 22, "int"),
            (26, 15, "const int"),
            (26, 22, "int"),
            (28,  5, "const int"),
            (29,  5, "int"),
            (35,  5, "const int"),
            (36,  5, "int"),
        )
    ),
    FileToTest(
        path="glsl-samples/hover/struct_fields.frag",
        hover_test_args=(
            (30,  5, "const AAA"),
            (31,  5, "const BBB"),
            (32,  5, "const CCC"),
            (35,  9, "int"),
            (36,  9, "float"),
            (37,  9, "AAA"),
            ExpectFail((40, 13, "int")),
            ExpectFail((41, 13, "float")),
            ExpectFail((42, 13, "uint")),
            ExpectFail((43, 13, "float")),
            ExpectFail((44, 13, "int")),
            ExpectFail((45, 13, "bool")),
        )
    )
]







# This is just setup.
@pytest_lsp.fixture(
    config=pytest_lsp.ClientServerConfig(
        server_command=["glsl_analyzer", "--stdio", "--clientProcessId", str(os.getpid())]
    )
)
async def client(lsp_client: pytest_lsp.LanguageClient):
    params = lspt.InitializeParams(
        capabilities=lspt.ClientCapabilities(),
        process_id=os.getpid()
    )
    await lsp_client.initialize_session(params)
    yield
    await lsp_client.shutdown_session()




class OpenedFile:
    def __init__(self, file: FileToTest, identifier: lspt.TextDocumentIdentifier):
        self.file       = file
        self.identifier = identifier

    def __str__(self) -> str:
        return self.file.path



@pytest.fixture(params=files)
def opened_file(client: pytest_lsp.LanguageClient, request):
    file: FileToTest = request.param
    fullpath         = base_directory / file.path

    with open(fullpath, "r") as file_handle:
        source = file_handle.read()

    this_doc = lspt.TextDocumentItem(
        uri=f"file://{fullpath}",
        language_id="glsl",
        version=0,
        text=source
    )

    this_doc_id = lspt.TextDocumentIdentifier(
        uri=this_doc.uri
    )

    client.text_document_did_open(
        lspt.DidOpenTextDocumentParams(this_doc)
    )

    yield OpenedFile(file, this_doc_id)

    client.text_document_did_close(
        lspt.DidCloseTextDocumentParams(this_doc_id)
    )





def to_zero_based_position(line: int, column: int) -> lspt.Position:
    return lspt.Position(line - 1, column -1)

def strip_md(mdtext: lspt.MarkupContent) -> str:
    text = mdtext.value
    text = re.sub("```glsl\n", "", text)
    text = re.sub("\n```",     "", text)
    return text



@pytest.mark.asyncio
async def test_hover(
    client: pytest_lsp.LanguageClient,
    opened_file: OpenedFile,
    subtests: pytest_subtests.SubTests
):

    for hover_args in opened_file.file.hover_test_args:
        line, column, expected, expect_success = unwrap_args(hover_args)
        file_location = f"{opened_file}:{line}:{column}"

        with subtests.test(msg=file_location):

            result = await client.text_document_hover_async(
                params=lspt.HoverParams(
                    text_document=opened_file.identifier,
                    position=to_zero_based_position(line, column)
                )
            )

            if result is not None:
                result = strip_md(result.contents)

            if expect_success:
                assert result == expected
            else: # expect fail
                # Ohh, how much do I not like this...
                assert result != expected, "Expected to fail, passed instead"
                pytest.xfail(file_location)



