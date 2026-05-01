#!/usr/bin/env bash
set -euo pipefail

# Dispatcher: pick the right Emacs screenshot backend for the current OS.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$(uname)" in
  Darwin)
    exec bash "$SCRIPT_DIR/emacs-screenshot-darwin.sh" "$@"
    ;;
  Linux)
    exec bash "$SCRIPT_DIR/emacs-screenshot-linux.sh" "$@"
    ;;
  *)
    echo "ERROR: Unsupported OS for Emacs screenshots: $(uname)" >&2
    exit 1
    ;;
esac
