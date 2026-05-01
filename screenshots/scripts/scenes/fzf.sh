#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# fzf-git-branch() picker from .zsh_custom/utils.zsh with git log preview.
launch_kitty "cd /tmp/showcase && exec zsh -i" 1200 800
sleep 2

type_keys "fzf-git-branch"
press Return
sleep 2

type_keys "feat"
sleep 2

capture "$OUTPUT_DIR/fzf.png"
