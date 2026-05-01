#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Neovim editing a Nix file with neo-tree sidebar (<Leader>op).
launch_kitty "cd /tmp/showcase && nvim config.nix" 1200 800
sleep 6

type_keys " op"
sleep 2

capture "$OUTPUT_DIR/neovim-editing.png"
