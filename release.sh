#!/usr/bin/env bash

targets=(
    x86_64-linux-musl
    aarch64-linux-musl
    x86_64-macos
    aarch64-macos
    x86_64-windows
    aarch64-windows
)

for target in ${targets[@]}; do
    mkdir -p "zig-out/$target"
    zig build -Dtarget=$target -Doptimize=ReleaseSafe --prefix "zig-out/$target" &
done

wait

mkdir -p "zig-out/archives"
for target in ${targets[@]}; do
    zip -r "zig-out/archives/$target.zip" zig-out/$target/*
done
