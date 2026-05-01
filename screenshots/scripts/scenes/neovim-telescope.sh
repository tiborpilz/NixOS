#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Telescope frecency finder (<Leader>pf) with a partial query.
launch_kitty "cd /tmp/showcase && nvim ." 1200 800
sleep 6

type_keys " pf"
sleep 2

type_keys "config"
sleep 2

capture "$OUTPUT_DIR/neovim-telescope.png"
