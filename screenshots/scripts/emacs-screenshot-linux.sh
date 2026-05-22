#!/usr/bin/env bash
set -euo pipefail

# Emacs GUI screenshot capture for Linux/CI using Xvfb + ImageMagick `import`.
# Self-wraps in xvfb-run if no X session is active.

# If we're not already inside an X session, start Xvfb with GLX/render
# extensions so Emacs's GUI frame can initialize.
if [[ -z "${DISPLAY:-}" ]]; then
  exec xvfb-run -a \
    -s '-screen 0 1600x1000x24 +extension GLX +render -noreset' \
    bash "$0" "$@"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/../output"
SETUP_EL="${SCRIPT_DIR}/emacs-setup.el"

mkdir -p "$OUTPUT_DIR"

if ! emacsclient --eval "(+ 1 1)" &>/dev/null; then
  echo "Starting Emacs daemon on DISPLAY=$DISPLAY..."
  emacs --daemon
  for _ in $(seq 1 60); do
    if emacsclient --eval "(+ 1 1)" &>/dev/null; then
      break
    fi
    sleep 1
  done
fi

emacsclient --eval "(load-file \"${SETUP_EL}\")"

emacsclient -c --eval '(progn (showcase--set-frame-size) (raise-frame))' &
sleep 5

find_emacs_window() {
  xdotool search --class Emacs 2>/dev/null | tail -1
}

capture_emacs_window() {
  local output_file="$1"
  local wid
  wid="$(find_emacs_window)"
  if [[ -z "$wid" ]]; then
    echo "ERROR: No Emacs window found on $DISPLAY" >&2
    return 1
  fi
  import -window "$wid" "$output_file"
  echo "Captured: $output_file"
}

echo "=== Emacs Dashboard ==="
emacsclient --eval '(showcase--show-dashboard)'
sleep 2
capture_emacs_window "$OUTPUT_DIR/emacs-dashboard.png"

echo "=== Emacs Org Mode ==="
emacsclient --eval '(showcase--show-org)'
sleep 2
capture_emacs_window "$OUTPUT_DIR/emacs-org.png"

# echo "=== Emacs Code + Treemacs ==="
# emacsclient --eval '(showcase--show-code-treemacs)'
# sleep 2
# capture_emacs_window "$OUTPUT_DIR/emacs-code-treemacs.png"

emacsclient --eval '(kill-emacs)' 2>/dev/null || true

echo "Emacs screenshots complete."
