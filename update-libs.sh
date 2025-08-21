#!/usr/bin/env bash

set -e

LIBS_PATH=$(nix build .#tree-sitter-smithyql-all --no-link --print-out-paths --print-build-logs)
mkdir -p modules/treesitter/src/main/resources
cp -R "$LIBS_PATH"/* modules/treesitter/src/main/resources
chmod -R 755 modules/treesitter/src/main/resources
