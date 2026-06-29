#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Tmux session with windows, splits, and gitmux status bar.
launch_kitty "cd /tmp/showcase && tmux new-session -s nixos -n Editor 'nvim config.nix'" 1200 800
sleep 4

# Tmux prefix is Ctrl+B (home-manager default; not overridden in tmux.nix).
press ctrl+b
press c
sleep 0.5

press ctrl+b
press comma
sleep 0.5
type_keys "Shell"
press Return
sleep 0.5

type_keys "cd /tmp/showcase && git log --oneline -5"
press Return
sleep 1

press ctrl+b
press 1
sleep 1

press ctrl+b
press quotedbl
sleep 1

type_keys "git diff --stat"
press Return
sleep 1

press ctrl+b
press k
sleep 1

capture "$OUTPUT_DIR/tmux.png" "tmux: nixos"
