#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Telescope frecency finder (<Leader>pf): open it, narrow on a query, widen, close.
launch_kitty "cd /tmp/showcase && nvim ." 1200 800
sleep 6

start_record
type_keys " pf"        # open telescope
sleep 1.5
type_keys "config"     # live-narrow the result list
sleep 1.5
press BackSpace BackSpace BackSpace
sleep 1.0
press Escape
sleep 0.5
stop_record "$OUTPUT_DIR/neovim-telescope.gif" "nvim — telescope"
