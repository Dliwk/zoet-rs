#/usr/bin/env bash
set -e

for i in zoet-macro zoet ; do
    cargo publish "$@" --manifest-path "$i"/Cargo.toml
done
