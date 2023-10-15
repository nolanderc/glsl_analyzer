#!/usr/bin/env bash


testdir="${BASH_SOURCE%/*}"

well_formed_dirs="
$testdir/glsl-samples/well-formed/
$testdir/glsl-samples/well-formed/glslang
"


failed=0

for d in $well_formed_dirs; do
    echo "================================================"
    "$testdir/run-parser-tests-well-formed.sh" "$d" || failed=1
done

if [[ $failed != 0 ]]; then
    exit 1
fi
