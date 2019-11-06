#/usr/bin/env bash
set -e

for i in */Cargo.toml ; do
    cargo publish "$@" --manifest-path "$i"
done
