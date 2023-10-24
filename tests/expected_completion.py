from dataclasses import dataclass
import typing
from _expected_utils import validate_any_expected


@dataclass
class Generic:
    """Default completion target with no special testing semantics.
    Checks if all the elements of `expected` are uniquely present in
    the completion list."""
    expected: tuple[str, ...]


# This has to contain all of the types in this module.
# We cannot do this programmatically, unfortunately.
#
# TODO: Switch to Union once there's more than one class here.
#       Wrapping in a typing.Type is needed for get_args() to return
#       an acutal type and not an empty tuple.
AnyExpected = typing.Type[Generic]

def is_any_expected(expected: AnyExpected) -> bool:
    return isinstance(expected, typing.get_args(AnyExpected))

validate_any_expected(__name__, AnyExpected)
