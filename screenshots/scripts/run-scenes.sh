#!/usr/bin/env bash
set -euo pipefail

# Run all terminal screenshot scenes. Each scene is a self-contained
# script under screenshots/scripts/scenes/<name>.sh that drives a real
# Kitty under Xvfb and captures a single PNG via ImageMagick.
#
# SCREENSHOTS_SKIP=name1,name2 to skip specific scenes.

# If we're not already inside an X session, start Xvfb with GLX/render
# extensions so Kitty's OpenGL context can initialize.
if [[ -z "${DISPLAY:-}" ]]; then
  exec xvfb-run -a \
    -s '-screen 0 1200x800x24 +extension GLX +render -noreset' \
    bash "$0" "$@"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCENES_DIR="${SCRIPT_DIR}/scenes"

SCENES=(
  "zsh-prompt"
  "fzf"
  "tmux"
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

# Pre-warm neovim plugins so individual nvim scenes don't pay for the sync.
echo "Syncing neovim plugins..."
nvim --headless "+Lazy! sync" +qa &>/dev/null || true

for scene in "${SCENES[@]}"; do
  if is_skipped "$scene"; then
    echo ""
    echo "--- ${scene} (skipped via SCREENSHOTS_SKIP) ---"
    continue
  fi
  scene_script="${SCENES_DIR}/${scene}.sh"
  if [[ ! -x "$scene_script" ]]; then
    echo "WARNING: scene script not found or not executable: $scene_script"
    continue
  fi
  echo ""
  echo "--- ${scene} ---"
  bash "$scene_script"
  echo "  Done: ${scene}"
done
