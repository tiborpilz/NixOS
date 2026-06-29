#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Custom zsh prompt with directory, git branch/status, and nix-shell indicator.
launch_kitty "cd /tmp/showcase && exec zsh -i" 1200 400
sleep 2

# Wipe shell-startup output (antigen apply, etc.) so only the prompt demo shows.
type_keys "clear"
press Return
sleep 0.5

type_keys "git status --short"
press Return
sleep 1

type_keys "ls"
press Return
sleep 1

type_keys "IN_NIX_SHELL=1 exec zsh -i"
press Return
sleep 2

type_keys "echo 'nix-shell active'"
press Return
sleep 1

capture "$OUTPUT_DIR/zsh-prompt.png" "zsh"
