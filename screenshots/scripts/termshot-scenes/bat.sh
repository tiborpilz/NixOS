#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Syntax-highlighted view of the mock Nix config (the Nord theme ties it to the
# rest of the setup). A focused line range keeps the image showcase-sized while
# still showing a function, string interpolation and the package list.
#   --paging=never  so bat never invokes a pager
#   --color=always  output goes to a file, so force color (no TTY to detect)
#   --style=numbers line numbers only - box-drawing-free, so termshot's bundled
#                   font renders every glyph (no Nerd Font icons involved)
cd "$SHOWCASE_DIR"

render bat "bat config.nix" -- \
  bat --paging=never --color=always --style=numbers --theme=Nord \
    --line-range=16:42 config.nix
