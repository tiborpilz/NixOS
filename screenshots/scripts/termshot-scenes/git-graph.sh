#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Colored commit graph across all branches of the mock repo. Output is captured
# to a file, so git won't auto-detect a TTY - force color explicitly.
cd "$SHOWCASE_DIR"

render git-graph "git log --graph --oneline --all --decorate" -- \
  git -c color.ui=always log --graph --oneline --all --decorate
