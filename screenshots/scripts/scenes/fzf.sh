#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# fzf-git-branch() picker from .zsh_custom/utils.zsh with git log preview.
launch_kitty "cd /tmp/showcase && exec zsh -i" 1200 800
sleep 2

start_record
type_keys "fzf-git-branch"
press Return
sleep 1.5
type_keys "feat"       # fuzzy-narrow the branch list, preview updates
sleep 1.5
press BackSpace BackSpace BackSpace BackSpace
sleep 1.0
press Escape
sleep 0.4
stop_record "$OUTPUT_DIR/fzf.gif" "fzf-git-branch"
