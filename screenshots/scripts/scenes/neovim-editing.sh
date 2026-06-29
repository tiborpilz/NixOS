#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Neovim with the neo-tree sidebar (<Leader>op): open it, move around, close it.
launch_kitty "cd /tmp/showcase && nvim config.nix" 1200 800
sleep 6

start_record
type_keys " op"        # toggle neo-tree open
sleep 1.2
press j; sleep 0.3; press j; sleep 0.3; press j
sleep 0.6
press k; sleep 0.3; press k
sleep 0.6
type_keys " op"        # toggle neo-tree closed
sleep 1.0
stop_record "$OUTPUT_DIR/neovim-editing.gif" "nvim config.nix"
