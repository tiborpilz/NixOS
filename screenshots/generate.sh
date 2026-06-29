#!/usr/bin/env bash
set -euo pipefail

# Local-dev orchestrator: runs the terminal scenes (under Xvfb on Linux) and
# the Emacs GUI screenshots sequentially. CI invokes the individual scripts
# directly to parallelize.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/output"
SCRIPTS_DIR="${SCRIPT_DIR}/scripts"

mkdir -p "$OUTPUT_DIR"

echo "=== Screenshot Generator ==="
echo ""

check_cmd() {
  if ! command -v "$1" &>/dev/null; then
    echo "ERROR: $1 not found. Run this inside the dev shell."
    exit 1
  fi
}
check_cmd nvim
check_cmd tmux
check_cmd git
check_cmd fzf
check_cmd termshot
check_cmd bat
check_cmd ffmpeg
check_cmd xdotool

echo ""
echo "=== Creating mock git repo ==="
bash "${SCRIPTS_DIR}/mock-git-repo.sh"

echo ""
echo "=== Generating command-output screenshots (termshot, no display) ==="
bash "${SCRIPTS_DIR}/run-termshot-scenes.sh"

echo ""
echo "=== Generating terminal screenshots (Kitty under Xvfb) ==="
bash "${SCRIPTS_DIR}/run-scenes.sh"

# echo ""
# echo "=== Generating Emacs GUI screenshots ==="
# bash "${SCRIPTS_DIR}/emacs-screenshot.sh"
#
echo ""
echo "=== Generated Screenshots ==="
echo ""

if ls "$OUTPUT_DIR"/*.png &>/dev/null; then
  for f in "$OUTPUT_DIR"/*.png; do
    size=$(du -h "$f" | cut -f1)
    echo "  ${size}  $(basename "$f")"
  done
  echo ""
  echo "Output directory: ${OUTPUT_DIR}"
else
  echo "WARNING: No screenshots were generated."
  echo "Check the output above for errors."
fi
