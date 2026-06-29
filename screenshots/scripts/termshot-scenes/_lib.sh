#!/usr/bin/env bash
# Common helpers for termshot-based screenshot scenes. Sourced by sibling
# scripts. Each scene script:
#   . _lib.sh
#   cd "$SHOWCASE_DIR"
#   render <output-name> "displayed command" -- actual command (color forced)
#
# Unlike the Kitty scenes in ../scenes, these need no X server: termshot
# renders the captured command output straight to a framed PNG. They're meant
# for *command-output* showcases (git history, diffs, syntax-highlighted files)
# - not interactive TUIs, which reset the cursor and confuse termshot.

# -E (errtrace) so the ERR trap is inherited by shell functions; otherwise a
# failure inside render() would abort the scene via `set -e` with no output.
set -Eeuo pipefail

SCENE_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS_DIR="$(cd "${SCENE_LIB_DIR}/.." && pwd)"
SCREENSHOTS_DIR="$(cd "${SCRIPTS_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${SCREENSHOTS_DIR}/.." && pwd)"
OUTPUT_DIR="${SCREENSHOTS_DIR}/output"

# The mock git repo created by ../mock-git-repo.sh; the git/bat scenes run here.
SHOWCASE_DIR="${SHOWCASE_DIR:-/tmp/showcase}"

mkdir -p "$OUTPUT_DIR"

if ! command -v termshot &>/dev/null; then
  echo "ERROR: termshot not found. Run this inside the dev shell (nix develop .#screenshots)." >&2
  exit 1
fi

# Muted-green prompt arrow, matching termshot's own --show-cmd styling. We draw
# our own prompt line (see render) rather than use --show-cmd so the displayed
# command stays clean - free of the color-forcing flags the capture needs.
PROMPT_ARROW=$'\033[38;5;71m\xe2\x86\x92\033[0m'

# render <output-name> "<displayed command>" -- <command> [args...]
#
# Runs <command> (whose colored output we capture to a file), prepends a
# synthetic "<arrow> <displayed command>" prompt line, and renders a PNG with
# termshot's --raw-read. We deliberately avoid termshot's execute path: when a
# fast command emits more than a screenful, that path can race the PTY drain
# and render a truncated screen. --raw-read consumes a complete file, so the
# result is deterministic - which matters because CI commits these PNGs.
render() {
  local outname="$1" display="$2"
  shift 2
  [[ "${1:-}" == "--" ]] && shift

  local ansi png
  ansi="$(mktemp --suffix=.ansi)"
  png="${OUTPUT_DIR}/${outname}.png"

  {
    [[ -n "$display" ]] && printf '%b %s\n' "$PROMPT_ARROW" "$display"
    "$@"
  } >"$ansi"

  termshot --raw-read "$ansi" -f "$png"
  rm -f "$ansi"
  echo "Captured: $png"
}
