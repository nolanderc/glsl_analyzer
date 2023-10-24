import typing

import lsprotocol.types as lspt
import expected_hover
import expected_completion
import typeguard


class ExpectFail:
    """A wrapper class that signals that a test with
    this particular set of arguments is expected to fail"""
    def __init__(self, *args, reason: str="(No Reason Specified)"):
        self.args   = args
        self.reason = reason


class HoverTestArgs:
    def __init__(
        self,
        line: int, column: int,
        expected: expected_hover.AnyExpected|None,
        *,
        _expect_fail: bool = False,
        _expect_fail_reason: str = ""
    ):
        self.args = (
            line, column, expected,
            _expect_fail, _expect_fail_reason
        )

    def __iter__(self):
        return self.args.__iter__()


class CompletionTestArgs:
    def __init__(
        self,
        line: int, column: int,
        expected: expected_completion.AnyExpected|None,
        *,
        _expect_fail: bool = False,
        _expect_fail_reason: str = ""
    ):
        self.args = (
            line, column, expected,
            _expect_fail, _expect_fail_reason
        )

    def __iter__(self):
        return self.args.__iter__()


@typeguard.typechecked
def _wrap_expected_hover(
    expected: str|expected_hover.AnyExpected|None
):
    """Wraps the `expected` tuple argument into `Generic`
    if it's not any of the `AnyExpected` types or `None`.

    Leaves the `expected` untouched otherwise."""
    if expected_hover.is_any_expected(expected):
        return expected
    elif expected is None:
        return expected
    else:
        return expected_hover.Generic(expected)



@typeguard.typechecked
def _wrap_expected_completion(
    expected: typing.Tuple[str, ...]|expected_completion.AnyExpected|None
):
    """Wraps the `expected` tuple argument into `Generic`
    if it's not any of the `AnyExpected` types or `None`.

    Leaves the `expected` untouched otherwise."""
    if expected_completion.is_any_expected(expected):
        return expected
    elif expected is None:
        return expected
    else:
        return expected_completion.Generic(expected)


ArgsTuple = typing.Tuple[int, int, typing.Any|None]


@typeguard.typechecked
def _prepare_args(
    args_type: type[HoverTestArgs|CompletionTestArgs],
    args: ArgsTuple|ExpectFail
):
    """Removes top-level `ExpectFail` if present,
    wraps the `expected` parameter into `Generic` if necessary,
    and packs the args tuple into the specified `args_type`
    along with the `expect_fail` info."""

    def wrap(args: ArgsTuple):
        line, column, expected = args
        if args_type is HoverTestArgs:
            return (line, column, _wrap_expected_hover(expected))
        elif args_type is CompletionTestArgs:
            return (line, column, _wrap_expected_completion(expected))

    if isinstance(args, ExpectFail):
        return args_type(
            *wrap(args.args),
            _expect_fail=True,
            _expect_fail_reason=args.reason
        )
    else:
        return args_type(*wrap(args))


class FileToTest:
    def __init__(
        self,
        path: str,
        hover_test_args: tuple[tuple|ExpectFail] = (),
        completion_test_args: tuple[tuple|ExpectFail] = (),
    ):
        self.path                 = path
        self.hover_test_args      = \
            tuple(_prepare_args(HoverTestArgs, args) for args in hover_test_args)
        self.completion_test_args = \
            tuple(_prepare_args(CompletionTestArgs, args) for args in completion_test_args)


class OpenedFile:
    def __init__(self, file: FileToTest, identifier: lspt.TextDocumentIdentifier):
        self.file       = file
        self.identifier = identifier

    def __str__(self) -> str:
        return self.file.path


