#!/usr/bin/env bash

set -e

rm -rf "zig-out/release"
rm -rf "zig-out/archives"

zig build release -Doptimize=ReleaseSafe --prefix "zig-out/release" --verbose

mkdir -p "zig-out/archives"

for target_path in zig-out/release/*; do
    target=$(basename "$target_path")
    echo "archiving $target..."
    (cd "zig-out/release/$target/" && zip -r "../../archives/$target.zip" *)
done
