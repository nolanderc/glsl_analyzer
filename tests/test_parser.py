import subprocess
from pathlib import Path

import pytest
import pytest_subtests


base_directory = Path(__file__).parent.resolve()

glsl_extensions = (
    ".glsl",  ".vert",  ".frag",  ".geom",
    ".comp",  ".tesc",  ".tese",  ".rgen",
    ".rint",  ".rahit", ".rchit", ".rmiss",
    ".rcall", ".mesh",  ".task"
)


@pytest.fixture(
    params=("glsl-samples/well-formed/glslang/",)
)
def dir_with_test_files(request) -> Path:
    return base_directory / Path(request.param)


def test_parser_in_directory(
    subtests: pytest_subtests.SubTests,
    dir_with_test_files: Path
):

    for file in dir_with_test_files.iterdir():
        if file.suffix not in glsl_extensions:
            continue

        with subtests.test(msg=str(file)):
            output = subprocess.run(
                args=("glsl_analyzer", "--parse-file", str(file)),
                capture_output=True,
                text=True
            )

            assert len(output.stderr) == 0, output.stderr
