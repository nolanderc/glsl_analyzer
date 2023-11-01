# `glsl_analyzer/tests`

- [How do I run the tests?](#how-do-i-run-the-tests)
- [LSP tests](#lsp-tests)
    - [How do I debug failing tests?](#how-do-i-debug-failing-tests)
    - [How do I add more files for testing / write new test cases?](#how-do-i-add-more-files-for-testing--write-new-test-cases)
        - [How tests and test input are structured](#how-tests-and-test-input-are-structured)
        - [Specifying expectations](#specifying-expectations)
        - [Expecting `None`](#expecting-none)
    - [How to interpret `expected` depending on the context / provide custom testing logic?](#how-to-interpret-expected-depending-on-the-context--provide-custom-testing-logic)
        - [Real type of `expected`](#real-type-of-expected)
        - [Customizing testing logic](#customizing-testing-logic)
    - [Expecting failure](#expecting-failure)
- [Parser tests](#parser-tests)


## How do I run the tests?

**Requirements:** `python 3.10` and `requirements.txt`.

You can run the tests by directly invoking `pytest`:

```sh
$ pytest tests/
```
or
```sh
$ cd tests/ && pytest
```

`pytest` will automatically pick up tests from modules that have names like `test_*.py` and contain functions `test_*` and classes `Test*`. More patterns are available, see the `pytest` docs.

If you want to run individual test files, just specify them as arguments:

```sh
$ pytest tests/test_lsp.py
```

*Caution: do not run `pytest` with the `--runxfail` flag on these tests. We do not use `xfail` in its conventional form in our test suite; using this flag will make the tests fail in unpredictable ways.*


## LSP tests


### How do I debug failing tests?

The current test routines are set up to output the glsl file location that was targeted in the test:

```
[glsl-samples/well-formed/basic.vert:18:19] SUBFAIL test_lsp.py::test_hover[opened_file0] - AssertionError: assert 'const vec4' == 'const vec3'
```

Some editors will allow you to directly go to that `file:line:column` by clicking on it.

In order to debug the test runtime, you can start by asking the `pytest` to print all the local variables of a failed context with:
```sh
$ pytest -l
```

In addition, custom `print` calls can be inserted into the test routine bodies. Their output is suppresed by `pytest` by default, but can be enabled by running:
```sh
$ pytest -s
```

As a last (and best) resort, pytest can drop you into a PDB debug session whenever a test has failed with:
```sh
$ pytest --pdb
```


### How do I add more files for testing / write new test cases?

#### How tests and test input are structured:

`pytest` is a fixture centered framework, so it's test specifications are much more convinient if they are described as data and dependencies. We do not write `test_*` functions for every case, instead we have a few *testing routines* like `test_hover` and `test_completion` that accept batches of input data and run subtests on the elements of each batch. Here's pseudocode that demonstates how the tests are run:

```py
def test_hover(file):
    for hover_input in file.hover_input_batch:
        subtest(hover_input)

for file in files_to_test:
    test_hover(file)
```

The test input is specified through the magical `files_to_test` variable. It is a simple tuple of `FileToTest` objects and can be found in the `lsp_testing_input.py` module.

In order to define a new file for testing, we create another instance of `FileToTest` in that tuple with the only mandatory argument being the path to the file on disk:

```py
files_to_test=(
    # ...
    FileToTest(
        path="path/relative/to/tests/dir/code.glsl"
    ),
    # ...
)
```

*Note that due to how fixtures work, this will already run `test_*` functions for this file and report success even though no actual tests will be performed.*

In order to run the tests, we have to fill in the batches of arguments for the testing routine:

```py
files_to_test=(
    # ...
    FileToTest(
        path="path/relative/to/tests/dir/code.glsl",
        hover_test_args=(
            (12,  5, "vec3"),
            (16,  5, "const int")
            # ...
        ),
        completion_test_args=(
            (12,  7, ("variable", "value")),
            (16, 16, ("iteration",))
            # ...
        ),
    ),
    # ...
)
```

Each element in a batch follows the format of:

```py
(line: int, column: int, expected: AnyExpected|<type of Generic.expected>|None)
```

where `line` and `column` are a *1-based* location in a file, and `expected` is a special parameter whose semantics depend on a test routine and optionally on a context of the request being made (see [How to provide custom testing logic?](#how-to-interpret-expected-depending-on-the-context--provide-custom-testing-logic) for more info).


#### Specifying expectations:

Let's take a look at the example above. One of the elements of the `hover_test_args` batch is:
```py
(16,  5, "const int")
```
This will make a `Hover` LSP request to the server, receive the result, strip it from the markdown formatting and do a *simple string comparison*. If the test file has these lines:

```glsl
14  const int value = 1;
15  void main() {
16      value;
17  }
```

we expect this test to pass.

The `completion_test_args` expects `expected` to be a tuple even if it contains a single element. Each element in the `expected` tuple will be compared against the completion list returned from the LSP request. For the test to pass, each element *has to be present* in the completion list, and it *has to be present only once (be unique)*.


#### Expecting `None`:

The type of `expected` can be `None` which can be used to signal that no information is expected to be received from the server on that request. Informally, this means that `client.*_request` call will have returned `None`.

For example, we can use that to test that hover does not provide anything in the comments:
```py
# ...
# Some word like "vec3" mentioned in a comment.
# Should not produce any hover information.
(42, 23, None),
# ...
```

### How to interpret `expected` depending on the context / provide custom testing logic?


#### Real type of `expected`:

The `expected` argument isn't actually a simple string or a tuple once it reaches the testing routine. The meaning of `expected` as it's explained above only covers the *most-likely* use-case. In reality, if left as is, the `expected` argument will be converted into the `Generic` type of the corresponding `expected_hover.py` or `expected_completion.py` module.

That means that specifying your tests as

```py
hover_test_args=(
    (23, 12, "const vec3"),
),
completion_test_args=(
    (25, 16, ("vec3", "vec4")),
)
```
is equivalent to
```py
hover_test_args=(
    (23, 12, expected_hover.Generic("const vec3")),
),
completion_test_args=(
    (25, 16, expected_completion.Generic(("vec3", "vec4"))),
)
```

The conversion to `expected_*.Generic` will only happend if the `expected` argument does not belong to a type found in the `expected_*` module (i.e. not part of `expected_*.AnyExpected` union).

The type of the `expected` will dictate the logic of the test and the accepted arguments (be it a string, a tuple, a set, etc.).

In the testing routine, the type of `expected` will be matched against the the set of types in `expected_*`, and the testing logic will branch based on that, as if:

```py
match (expected):
    case Generic():
        expect_generic(expected, result)
    case VarDecl():
        expect_var_decl(expected, result)
    case FunctionIdent():
        expect_function_ident(expected, result)
    # etc...
```


#### Customizing testing logic:

In order to provide custom behavior for your test, follow these steps:

1. Inspect `expected_*` modules too see if the behavior / context you're looking for has already been implemented. If it has, use it.
2. If the behavior you need isn't already present, create a dataclass for that behavior and provide a short description.
3. Add the new class to the `AnyExpected` union of your `expected_*` module for correct type validation.
4. In the testing routine inside `test_lsp.py`, add a case for your new class that branches off into the custom testing logic.
    ```py
    match (expected):
        case expected_hover.Generic():
            expect_generic(expected.expected, result)
        case expected_hover.FunctionIdent():
            expect_function(expected.expected, result)
        # ...
        case expected_hover.MyCustomContext():
            expect_my_custom_context(expected.expected, result)
        # ...
    ```
5. When writing you custom logic, take note of the existing implementations. Don't forget to handle expected-to-fail and `None` cases.

Once that's done, wrap your `expected` argument into your new type:

```py
hover_test_args=(
    (12, 13, "const vec3"),
    (14, 13, expected_hover.FunctionIdent({"int (int)", "uint (uint)"})),
    (15, 13, expected_hover.MyCustomContext("my_expected_result")),
    # ...
),
```


*It is not recommended to add a custom `expected_*` type if the `Generic` type already satisfies your requirements just because you want to be more explicit about the token you're testing. That is, a custom `VarIdent` is not a good idea because it's testing logic would be exactly the same as `Generic`. At the same time, reusing any non-`Generic` type for a different token/context is confusing and is a bad idea. Create a new type but reuse the testing logic in that case instead.*


### Expecting failure

In order to expect failure of certain test cases,
we provide a special `ExpectFail` wrapper around the usual argument tuple. The cases that are expected to fail are specified as follows:

```py
hover_test_args=(
    ExpectFail(12, 13, "const vec3", reason="Known bug"),
    ExpectFail(15, 13, expected_hover.FunctionIdent("int (int)"), \
        reason="Known bug. Current version uses simple string comparison for removing overloads")
),
```

We always expect failure in *strict mode*: if the test was expected to fail, but passed instead, this signals a test failure. This forces you to update your tests after fixing the bugs to be up-to-date with the current state.




## Parser tests

Parser tests are simple pytest scripts that run the parser with `glsl_analyzer --parse-file` on a set of files. Currently we only test successful parsing on well-formed code and check that the `glsl_analyzer` did not output anything into `stderr`.

Run only the parser tests by executing

```sh
$ pytest test_parser.py
```

You can add more directories for testing by adding them to the  `dir_with_test_files` fixture `params` in `test_parser.py`.

