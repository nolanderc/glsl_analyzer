import typing
from enum import Enum

import lsprotocol.types as lspt


class ExpectFail:
    """A wrapper class that signals that a test with
    this particular set of arguments is expected to fail"""
    def __init__(self, *args, reason: str="(No Reason Specified)"):
        self.args   = args
        self.reason = reason


class TokenKind(Enum):
    """Enum used for differentiating between formats
    of hover info, completion results, etc.

    The interpretation of the `expected` parameter changes
    based on the supplied `TokenKind`.

    Most of the time the default `Other` kind is enough,
    but some kinds like `Function` require special handling
    because of overloads, for example."""

    Other = 0
    """Default token kind with no special testing semantics.

    For hover:
        `expected` is `str|None`.
        Compares stripped version of result with `expected`.

    For completion:
        `expected` is `tuple[str]|None`.
        Checks if all the elements of `expected` are uniquely present in
        the completion list."""

    Function = 1
    """Special token kind capable of handling the function overloads.

    For hover:
        `expected` is `str|set[str]|None`.
        Checks that each overload of the `expected` set is present
        in the stripped result.

    For completion:
        Same as `Other`."""


class HoverTestArgs:
    def __init__(
        self,
        line: int, column: int,
        expected,
        token_kind: TokenKind = TokenKind.Other,
        *,
        _expect_fail: bool = False,
        _expect_fail_reason: str = ""
    ):
        self.args = (
            line, column, expected, token_kind,
            _expect_fail, _expect_fail_reason
        )

    def __iter__(self):
        return self.args.__iter__()


class CompletionTestArgs:
    def __init__(
        self,
        line: int, column: int,
        expected,
        token_kind: TokenKind = TokenKind.Other,
        *,
        _expect_fail: bool = False,
        _expect_fail_reason: str = ""
    ):
        self.args = (
            line, column, expected, token_kind,
            _expect_fail, _expect_fail_reason
        )

    def __iter__(self):
        return self.args.__iter__()


def _unwrap_args(
    args_type: type[HoverTestArgs|CompletionTestArgs],
    args: tuple|ExpectFail
):
    """Removes top-level `ExpectFail` if present
    and packs the args tuple into the specified `args_type`
    along with the `expect_fail` info."""
    if isinstance(args, ExpectFail):
        return args_type(
            *args.args,
            _expect_fail=True,
            _expect_fail_reason=args.reason
        )
    elif isinstance(args, tuple):
        return args_type(*args)
    else:
        raise TypeError(
            "Expected tuple or an instance of ExpectArgs."
        )


class FileToTest:
    def __init__(
        self,
        path: str,
        hover_test_args: tuple[tuple|ExpectFail] = (),
        completion_test_args: tuple[tuple|ExpectFail] = (),
    ):
        self.path                 = path
        self.hover_test_args      = \
            tuple(_unwrap_args(HoverTestArgs, args) for args in hover_test_args)
        self.completion_test_args = \
            tuple(_unwrap_args(CompletionTestArgs, args) for args in completion_test_args)


class OpenedFile:
    def __init__(self, file: FileToTest, identifier: lspt.TextDocumentIdentifier):
        self.file       = file
        self.identifier = identifier

    def __str__(self) -> str:
        return self.file.path


