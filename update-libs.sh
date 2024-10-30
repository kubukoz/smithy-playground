#!/usr/bin/env bash

set -e

mkdir -p modules/parser/src/main/resources/darwin-aarch64
BINARY_PATH=$(nix build .#packages.aarch64-darwin.tree-sitter-smithyql --no-link --print-out-paths --print-build-logs)
cp $BINARY_PATH modules/parser/src/main/resources/darwin-aarch64/libtree-sitter-smithyql.dylib
chmod 755 modules/parser/src/main/resources/darwin-aarch64/libtree-sitter-smithyql.dylib

mkdir -p modules/parser/src/main/resources/darwin-x86-64
BINARY_PATH=$(nix build .#packages.x86_64-darwin.tree-sitter-smithyql --no-link --print-out-paths --print-build-logs)
cp $BINARY_PATH modules/parser/src/main/resources/darwin-x86-64/libtree-sitter-smithyql.dylib
chmod 755 modules/parser/src/main/resources/darwin-x86-64/libtree-sitter-smithyql.dylib

mkdir -p modules/parser/src/main/resources/linux-aarch64
BINARY_PATH=$(nix build .#packages.aarch64-linux.tree-sitter-smithyql --no-link --print-out-paths --print-build-logs)
cp $BINARY_PATH modules/parser/src/main/resources/linux-aarch64/libtree-sitter-smithyql.so
chmod 755 modules/parser/src/main/resources/linux-aarch64/libtree-sitter-smithyql.so

mkdir -p modules/parser/src/main/resources/linux-x86-64
BINARY_PATH=$(nix build .#packages.x86_64-linux.tree-sitter-smithyql --no-link --print-out-paths --print-build-logs)
cp $BINARY_PATH modules/parser/src/main/resources/linux-x86-64/libtree-sitter-smithyql.so
chmod 755 modules/parser/src/main/resources/linux-x86-64/libtree-sitter-smithyql.so
