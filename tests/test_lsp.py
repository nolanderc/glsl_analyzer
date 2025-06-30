import os
import pathlib
import re
import typing

import lsprotocol.types as lspt

import pytest
import pytest_subtests
import pytest_lsp


from testing_utils import (
    FileToTest,
    OpenedFile,
)

import lsp_testing_input
import expected_hover
import expected_completion



@pytest_lsp.fixture(config=pytest_lsp.ClientServerConfig(
    server_command=["glsl_analyzer", "--stdio", "--clientProcessId", str(os.getpid())]
))
async def client(lsp_client: pytest_lsp.LanguageClient):
    """A fixtre that carries a fake client."""
    params = lspt.InitializeParams(
        capabilities=lspt.ClientCapabilities(),
        process_id=os.getpid()
    )
    await lsp_client.initialize_session(params)
    yield
    await lsp_client.shutdown_session()


@pytest.fixture(params=lsp_testing_input.files_to_test)
def opened_file(client: pytest_lsp.LanguageClient, request):
    file: FileToTest = request.param
    fullpath         = lsp_testing_input.base_directory / file.path

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


def none_guard(
    expected: typing.Any|None, result: typing.Any|None,
    expect_fail: bool, expect_fail_reason: str, context: str
) -> bool:
    """Check if either `expected` or `result` is `None`.
    If so, validate expectations.

    If this returns `True`, both `expected` and `result` are `None` and
    no further assertions should be made.

    If this returns `False`, both `expected` and `result` have values
    that should be validated further.

    Otherwise, the function will exit through `assert` or `xfail`.
    """

    if expected is None or result is None:
        if not expect_fail:
            assert result == expected
            return True
        else:
            assert result != expected, \
                f"Expected to fail because: {expect_fail_reason}. Passed instead."
            pytest.xfail(context) # raises exception
    return False


def strip_until_naked(formatted_text: str) -> str:
    strippables = ("```glsl", "```", "\n", " ")

    post_strip = formatted_text
    while True:
        pre_strip = post_strip
        # This is probably inefficient, but I don't care.
        # Keep strippin'.
        for markdown_garbage in strippables:
            post_strip = post_strip \
                .removeprefix(markdown_garbage) \
                .removesuffix(markdown_garbage)

        if pre_strip == post_strip:
            break

    return post_strip




@pytest.mark.asyncio
async def test_hover(
    client: pytest_lsp.LanguageClient,
    opened_file: OpenedFile,
    subtests: pytest_subtests.SubTests
):

    for args in opened_file.file.hover_test_args:
        line, column, expected, \
            expect_fail, expect_fail_reason = args

        file_location = f"{opened_file}:{line}:{column}"


        def expect_generic(expected: str, result: lspt.Hover):

            result: str = strip_until_naked(result.contents.value)

            if not expect_fail:
                assert result == expected
            else: # expect fail
                assert result != expected, \
                    f"Expected to fail because: {expect_fail_reason}. Passed instead."
                pytest.xfail(file_location)


        def expect_function(expected: str|set[str], result: lspt.Hover):
            def strip_md(mdtext: lspt.MarkupContent) -> list[str]:

                text = strip_until_naked(mdtext.value)
                entries = text.split(sep="\n")

                return entries


            result: list[str] = strip_md(result.contents)

            expected_set = {expected} if isinstance(expected, str) else expected
            result_set   = set(result)

            has_no_duplicates        = len(result_set) == len(result)
            is_expected_overload_set = expected_set == result_set

            if not expect_fail:
                assert has_no_duplicates and is_expected_overload_set
            else:
                assert not (has_no_duplicates and is_expected_overload_set), \
                    f"Expected to fail because: {expect_fail_reason}. Passed instead."
                pytest.xfail(file_location)




        with subtests.test(msg=file_location):

            result = await client.text_document_hover_async(
                params=lspt.HoverParams(
                    text_document=opened_file.identifier,
                    position=to_zero_based_position(line, column)
                )
            )

            if none_guard(expected, result, expect_fail, expect_fail_reason, file_location):
                continue

            match (expected):
                case expected_hover.Generic():
                    expect_generic(expected.expected, result)
                case expected_hover.FunctionIdent():
                    expect_function(expected.expected, result)
                case _:
                    assert False, \
                        "Unknown hover target type. Forgot to add a handler here?"






@pytest.mark.asyncio
async def test_completion(
    client: pytest_lsp.LanguageClient,
    opened_file: OpenedFile,
    subtests: pytest_subtests.SubTests
):

    for args in opened_file.file.completion_test_args:
        line, column, expected, \
            expect_fail, expect_fail_reason = args

        file_location = f"{opened_file}:{line}:{column}"


        def expect_generic(expected: tuple[str], result: list[lspt.CompletionItem]):
            labels = tuple(item.label for item in result)

            expected_set = set(expected)
            labels_set   = set(labels)

            def unique_if_present_in(label: str, collection):
                return sum(label == item for item in collection) <= 1

            all_expected_are_present = expected_set.issubset(labels_set)
            all_expected_are_unique  = all(unique_if_present_in(expected_label, labels) for expected_label in expected)

            if not expect_fail:
                # Check for uniqueness separately from presence,
                # so that assert fails would point to the right reason.
                assert all_expected_are_unique and all_expected_are_present
            else: # expect fail
                assert not (all_expected_are_unique and all_expected_are_present), \
                    f"Expected to fail because: {expect_fail_reason}. Passed instead."
                pytest.xfail(file_location)




        with subtests.test(msg=file_location):

            result = await client.text_document_completion_async(
                lspt.CompletionParams(
                    text_document=opened_file.identifier,
                    position=to_zero_based_position(line, column),
                    context=lspt.CompletionContext(
                        lspt.CompletionTriggerKind.Invoked
                    )
                )
            )

            if none_guard(expected, result, expect_fail, expect_fail_reason, file_location):
                continue

            match (expected):
                case expected_completion.Generic():
                    expect_generic(expected.expected, result)
                case _:
                    assert False, \
                        "Unknown completion target type. Forgot to add a handler here?"


@pytest.mark.asyncio
async def test_formatting_tab_size(client: pytest_lsp.LanguageClient):
    """Test that formatting respects the tabSize option."""

    # Create a simple GLSL document that needs formatting
    source = "void main() { int x = 1; int y = 2; }"

    doc = lspt.TextDocumentItem(
        uri="file:///test.glsl", language_id="glsl", version=0, text=source
    )

    # Open the document
    client.text_document_did_open(lspt.DidOpenTextDocumentParams(doc))

    try:
        # Test with default tab size (4 spaces)
        result_4 = await client.text_document_formatting_async(
            lspt.DocumentFormattingParams(
                text_document=lspt.TextDocumentIdentifier(uri=doc.uri),
                options=lspt.FormattingOptions(tab_size=4, insert_spaces=True),
            )
        )

        # Test with 2 spaces
        result_2 = await client.text_document_formatting_async(
            lspt.DocumentFormattingParams(
                text_document=lspt.TextDocumentIdentifier(uri=doc.uri),
                options=lspt.FormattingOptions(tab_size=2, insert_spaces=True),
            )
        )

        # Test with 8 spaces
        result_8 = await client.text_document_formatting_async(
            lspt.DocumentFormattingParams(
                text_document=lspt.TextDocumentIdentifier(uri=doc.uri),
                options=lspt.FormattingOptions(tab_size=8, insert_spaces=True),
            )
        )

        # Verify results
        assert result_4 is not None
        assert result_2 is not None
        assert result_8 is not None

        # Check that the formatted text has different indentation
        text_4 = result_4[0].new_text
        text_2 = result_2[0].new_text
        text_8 = result_8[0].new_text

        # All should format the code properly but with different indentation
        expected_4 = """void main() {
    int x = 1;
    int y = 2;
}
"""
        expected_2 = """void main() {
  int x = 1;
  int y = 2;
}
"""
        expected_8 = """void main() {
        int x = 1;
        int y = 2;
}
"""

        assert text_4 == expected_4
        assert text_2 == expected_2
        assert text_8 == expected_8

    finally:
        # Close the document
        client.text_document_did_close(
            lspt.DidCloseTextDocumentParams(lspt.TextDocumentIdentifier(uri=doc.uri))
        )
