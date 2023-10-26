from dataclasses import dataclass
import typing
from _expected_utils import validate_any_expected


@dataclass
class Generic:
    """Default hover target with no special testing semantics.
    Compares stripped version of result with `expected`."""
    expected: str

@dataclass
class FunctionIdent:
    """Special hover target capable of handling function overloads.
    Checks that each overload of the `expected` set is present
    in the stripped result."""
    expected: str|set[str]


# This has to contain all of the types in this module.
# We cannot do this programmatically, unfortunately.
AnyExpected = Generic | FunctionIdent

def is_any_expected(expected: AnyExpected) -> bool:
    return isinstance(expected, typing.get_args(AnyExpected))

validate_any_expected(__name__, AnyExpected)
