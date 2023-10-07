#!/usr/bin/env bash

set -e

targets=(
    x86_64-linux-musl
    aarch64-linux-musl
    x86_64-macos
    aarch64-macos
    x86_64-windows
    aarch64-windows
)

mkdir -p "zig-out/archives"

for target in ${targets[@]}; do
    echo "building $target..."
    mkdir -p "zig-out/$target"
    zig build -Dtarget=$target -Doptimize=ReleaseSafe --prefix "zig-out/$target"
    (cd "zig-out/$target/" && zip -r "../archives/$target.zip" *)
done
