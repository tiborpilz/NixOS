#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/output"
TAPES_DIR="${SCRIPT_DIR}/tapes"
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

# Check neovim config exists
if [[ -d "${HOME}/.config/nvim" ]]; then
  echo "  Neovim config found."
else
  echo "WARNING: ~/.config/nvim not found. Neovim screenshots may not show plugins."
fi

# Ensure neovim plugins are installed
echo "  Syncing neovim plugins (this may take a moment)..."
nvim --headless "+Lazy! sync" +qa &>/dev/null || true
echo "  Neovim plugins synced."

# --- Create mock repo ---

echo ""
echo "=== Creating mock git repo ==="
bash "${SCRIPTS_DIR}/mock-git-repo.sh"

# --- VHS tape screenshots ---

echo ""
echo "=== Generating terminal screenshots with VHS ==="

TAPES=(
  "zsh-prompt"
  "fzf"
  "tmux"
  "neovim-dashboard"
  "neovim-editing"
  "neovim-telescope"
)

for tape in "${TAPES[@]}"; do
  tape_file="${TAPES_DIR}/${tape}.tape"
  if [[ -f "$tape_file" ]]; then
    echo ""
    echo "--- ${tape} ---"
    # VHS outputs relative to its working directory
    (cd "$SCRIPT_DIR" && vhs "$tape_file")
    echo "  Done: ${tape}"
  else
    echo "WARNING: Tape file not found: ${tape_file}"
  fi
done

# --- Emacs GUI screenshots (macOS only) ---

echo ""
if [[ "$(uname)" == "Darwin" ]]; then
  echo "=== Generating Emacs GUI screenshots ==="
  bash "${SCRIPTS_DIR}/emacs-screenshot.sh"
else
  echo "SKIP: Emacs GUI screenshots (macOS only)"
fi

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
