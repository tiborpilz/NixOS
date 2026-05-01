#!/usr/bin/env bash
set -euo pipefail

# Run the VHS terminal tapes. Honors SCREENSHOTS_SKIP=tape-name,tape-name to
# skip individual tapes (e.g. SCREENSHOTS_SKIP=neovim-dashboard).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCREENSHOTS_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
TAPES_DIR="${SCREENSHOTS_DIR}/tapes"
OUTPUT_DIR="${SCREENSHOTS_DIR}/output"

mkdir -p "$OUTPUT_DIR"

TAPES=(
  "zsh-prompt"
  "fzf"
  "tmux"
  "neovim-dashboard"
  "neovim-editing"
  "neovim-telescope"
)

SKIP_RAW="${SCREENSHOTS_SKIP:-}"
IFS=',' read -ra SKIP_LIST <<<"$SKIP_RAW"
is_skipped() {
  local needle="$1"
  for s in "${SKIP_LIST[@]}"; do
    [[ -n "$s" && "$s" == "$needle" ]] && return 0
  done
  return 1
}

# Sync neovim plugins (idempotent; uses cache when warm).
echo "Syncing neovim plugins..."
nvim --headless "+Lazy! sync" +qa &>/dev/null || true

for tape in "${TAPES[@]}"; do
  tape_file="${TAPES_DIR}/${tape}.tape"
  if is_skipped "$tape"; then
    echo ""
    echo "--- ${tape} (skipped via SCREENSHOTS_SKIP) ---"
    continue
  fi
  if [[ -f "$tape_file" ]]; then
    echo ""
    echo "--- ${tape} ---"
    (cd "$SCREENSHOTS_DIR" && vhs "$tape_file")
    echo "  Done: ${tape}"
  else
    echo "WARNING: Tape file not found: ${tape_file}"
  fi
done
