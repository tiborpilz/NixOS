#!/usr/bin/env bash
set -euo pipefail

# Local-dev orchestrator: runs everything sequentially. CI invokes the
# individual scripts directly to parallelize.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/output"
SCRIPTS_DIR="${SCRIPT_DIR}/scripts"

mkdir -p "$OUTPUT_DIR"

echo "=== Screenshot Generator ==="
echo ""

# --- Pre-checks ---

echo "Running pre-checks..."
check_cmd() {
  if ! command -v "$1" &>/dev/null; then
    echo "ERROR: $1 not found. Run this inside nix-shell."
    exit 1
  fi
}
check_cmd vhs
check_cmd nvim
check_cmd tmux
check_cmd git
check_cmd fzf
echo "  All required tools found."

if [[ ! -d "${HOME}/.config/nvim" ]]; then
  echo "WARNING: ~/.config/nvim not found. Neovim screenshots may not show plugins."
fi

# --- Create mock repo ---

echo ""
echo "=== Creating mock git repo ==="
bash "${SCRIPTS_DIR}/mock-git-repo.sh"

# --- VHS tape screenshots ---

echo ""
echo "=== Generating terminal screenshots with VHS ==="
bash "${SCRIPTS_DIR}/run-tapes.sh"

# --- Emacs GUI screenshots ---

echo ""
echo "=== Generating Emacs GUI screenshots ==="
bash "${SCRIPTS_DIR}/emacs-screenshot.sh"

# --- Summary ---

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
