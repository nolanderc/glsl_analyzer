#!/usr/bin/env bash


print_usage() {
    echo "Usage: run-parser-tests.sh [directory]"
}

if [[ -z "$1" || -n "$2" ]]; then
    print_usage
    exit 1
fi

if [[ ! -d "$1" ]]; then
    echo "\"$1\" is not an existing directory"
    print_usage
    exit 1
fi

echo "Running parser tests in \"$1\" for glsl_analyzer $(glsl_analyzer --version)"

num_files=0
num_failed=0

for file in "$1"/*; do
    if [[ -f "$file" ]]; then

        # Filter out headers, shell scripts, etc.
        ext="${file##*.}"
        if [[
            $ext == "glsl"  || $ext == "vert"  || $ext == "frag"  || $ext == "geom"  ||
            $ext == "comp"  || $ext == "tesc"  || $ext == "tese"  || $ext == "rgen"  ||
            $ext == "rint"  || $ext == "rahit" || $ext == "rchit" || $ext == "rmiss" ||
            $ext == "rcall" || $ext == "mesh"  || $ext == "task"
        ]]; then

            # Redirect stderr to stdout, capture stdout into a variable,
            # then drop stdout into /dev/null.
            out=$(glsl_analyzer --parse-file "$file" 2>&1 >/dev/null)

            # If the output isn't empty, then the parser emitted an error.
            # We expect the test code to be well-formed and not produce erros,
            # so this is considered a test failure.
            if [[ -n "$out" ]]; then
                first_line=$(echo "$out" | head -1)
                let num_failed=num_failed+1
                echo "FAILED($(printf "%03d" "$num_failed")): $first_line"
            fi
            let num_files=num_files+1

        fi
    fi
done

echo
echo "FAILED $num_failed out of $num_files files."

if [[ $num_failed == 0 ]]; then
    echo "All tests passed."
else
    exit 1
fi
