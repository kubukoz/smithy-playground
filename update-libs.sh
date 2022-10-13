#!/bin/bash

set -e

mkdir -p core/src/main/resources/

RESULT=$(nix build ".#grammar-all" --no-link --print-out-paths --print-build-logs)
# copy, flattening links
cp --no-preserve=mode,ownership -Lr "$RESULT"/* core/src/main/resources/
