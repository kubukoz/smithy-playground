#!/bin/bash

set -e

function update_grammar() {
  OUT_DIR="core/src/main/resources/$2"
  OUT="$OUT_DIR/libtree-sitter-smithyql.$3"
  mkdir -p "$OUT_DIR"
  cp "$(nix build ".#packages.$1.grammar" --no-link --print-out-paths)/parser" "$OUT"
  chmod +w "$OUT"
}

update_grammar x86_64-darwin darwin-x86_64 dylib
update_grammar aarch64-darwin darwin-aarch64 dylib
update_grammar x86_64-linux linux-x86_64 so
update_grammar aarch64-linux linux-aarch64 so
