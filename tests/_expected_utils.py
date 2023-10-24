import sys, inspect, typing


# Yes, we validate the type alias that is used for validation later.
# Yes, I also considered testing the testing framework itself. It's a good idea, I agree.
def validate_any_expected(module_name: str, any_expected: typing.TypeAlias):
    types_in_module = set(c[1] for c in inspect.getmembers(sys.modules[module_name], inspect.isclass))
    types_in_any_expected = set(typing.get_args(any_expected))
    assert types_in_module == types_in_any_expected, \
        f"{module_name}.AnyExpected types ({types_in_any_expected}) are not the same as the types in the module {module_name} ({types_in_module})."

