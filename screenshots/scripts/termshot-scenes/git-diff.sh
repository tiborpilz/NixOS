#!/usr/bin/env bash
. "$(dirname "${BASH_SOURCE[0]}")/_lib.sh"

# Working-tree diff (mock-git-repo.sh leaves the tree dirty). Capturing to a
# file means no TTY, so force color and disable the pager explicitly.
cd "$SHOWCASE_DIR"

render git-diff "git diff" -- \
  git -c color.ui=always --no-pager diff
