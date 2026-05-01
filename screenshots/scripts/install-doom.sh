#!/usr/bin/env bash
set -euo pipefail

# Install Doom Emacs into $HOME/.config/emacs and run a sync against the
# in-repo Doom config (already symlinked at $HOME/.config/doom by
# setup-ci-configs.sh). Designed for CI: re-running with a warm
# ~/.config/emacs/.local cache only does an incremental sync.

EMACS_CONFIG_DIR="${EMACS_CONFIG_DIR:-$HOME/.config/emacs}"

if [[ ! -d "$EMACS_CONFIG_DIR" ]]; then
  echo "Cloning Doom Emacs into $EMACS_CONFIG_DIR..."
  git clone --depth=1 https://github.com/doomemacs/doomemacs "$EMACS_CONFIG_DIR"
fi

if [[ ! -d "$EMACS_CONFIG_DIR/.local" ]]; then
  echo "Running first-time doom install..."
  "$EMACS_CONFIG_DIR/bin/doom" --force install </dev/null
else
  echo "Running doom sync..."
  "$EMACS_CONFIG_DIR/bin/doom" sync </dev/null
fi
