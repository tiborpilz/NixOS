#!/usr/bin/env bash
set -euo pipefail

# Symlink the in-repo neovim and Doom configs into $HOME so the screenshot
# scripts pick them up. Idempotent: skips if the link already points where
# we want.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

mkdir -p "$HOME/.config"

link() {
  local src="$1"
  local dst="$2"
  if [[ -L "$dst" && "$(readlink "$dst")" == "$src" ]]; then
    echo "Already linked: $dst -> $src"
    return
  fi
  if [[ -e "$dst" ]]; then
    echo "ERROR: $dst exists and is not the expected symlink. Aborting." >&2
    exit 1
  fi
  ln -s "$src" "$dst"
  echo "Linked: $dst -> $src"
}

link "$REPO_ROOT/home/config/neovim" "$HOME/.config/nvim"
link "$REPO_ROOT/home/config/doom" "$HOME/.config/doom"
