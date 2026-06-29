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

# Resolve the first matching home-manager-generated file (given one or more
# candidate config attrs) and symlink it into place, so CI uses the user's real
# generated config rather than tool defaults.
extract_hm() {
  local dst="$1"; shift
  local attr path=""
  for attr in "$@"; do
    path=$(nix eval --raw "$REPO_ROOT#homeConfigurations.tibor.config.${attr}" \
      2>/dev/null || true)
    [[ -n "$path" ]] && break
  done
  if [[ -z "$path" ]]; then
    echo "WARNING: could not resolve any of: $*; skipping $dst" >&2
    return
  fi
  mkdir -p "$(dirname "$dst")"
  rm -f "$dst"
  ln -s "$path" "$dst"
  echo "Linked: $dst -> $path"
}

link "$REPO_ROOT/home/config/neovim" "$HOME/.config/nvim"
link "$REPO_ROOT/home/config/doom" "$HOME/.config/doom"

ZDOTDIR="$HOME/.config/zsh"
mkdir -p "$ZDOTDIR"
link "$REPO_ROOT/home/config/zsh/.zshrc" "$ZDOTDIR/.zshrc"
link "$REPO_ROOT/home/config/zsh/.zsh_custom" "$ZDOTDIR/.zsh_custom"

cat >"$HOME/.zshenv" <<EOF
export ZDOTDIR="$ZDOTDIR"
EOF
echo "Wrote $HOME/.zshenv (ZDOTDIR=$ZDOTDIR)"

# Extract kitty.conf from the home-manager config so CI uses the user's
# actual generated config (Nord theme, Fira Code, settings, extras) rather
# than a hand-maintained mirror.
echo "Extracting kitty.conf from homeConfigurations.tibor..."
mkdir -p "$HOME/.config/kitty"
KITTY_CONF=$(nix eval --raw \
  "$REPO_ROOT#homeConfigurations.tibor.config.xdg.configFile.\"kitty/kitty.conf\".source")
rm -f "$HOME/.config/kitty/kitty.conf"
ln -s "$KITTY_CONF" "$HOME/.config/kitty/kitty.conf"
echo "Linked: $HOME/.config/kitty/kitty.conf -> $KITTY_CONF"

# .zshrc sources $ZDOTDIR/extra.zshrc (home-manager-generated) at the end;
# create an empty stub so the source line doesn't error in CI.
if [[ ! -e "$ZDOTDIR/extra.zshrc" ]]; then
  touch "$ZDOTDIR/extra.zshrc"
  echo "Created empty stub: $ZDOTDIR/extra.zshrc"
fi

# Pre-warm antigen (clone bundles, build cache) so the interactive shells in
# the screenshot scenes start silently, without install messages or the
# first-run bootstrap's cd back to $HOME.
echo "Warming antigen cache..."
zsh -ic 'exit 0' </dev/null >/tmp/antigen-warm.log 2>&1 || true
echo "Antigen warm-up done."
