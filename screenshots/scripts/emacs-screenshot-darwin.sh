#!/usr/bin/env bash
set -euo pipefail

# Emacs GUI screenshot capture for macOS using emacsclient + screencapture.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/../output"
SETUP_EL="${SCRIPT_DIR}/emacs-setup.el"

mkdir -p "$OUTPUT_DIR"

if ! emacsclient --eval "(+ 1 1)" &>/dev/null; then
  echo "Starting Emacs daemon..."
  emacs --daemon 2>/dev/null
  for _ in $(seq 1 30); do
    if emacsclient --eval "(+ 1 1)" &>/dev/null; then
      break
    fi
    sleep 1
  done
fi

emacsclient --eval "(load-file \"${SETUP_EL}\")"

emacsclient --eval '(make-frame-visible (selected-frame))' 2>/dev/null || true
emacsclient -c --eval '(showcase--set-frame-size)' &
sleep 5

capture_emacs_window() {
  local output_file="$1"
  local window_id
  window_id=$(osascript -e '
    tell application "System Events"
      tell process "Emacs"
        set frontmost to true
        delay 0.5
      end tell
    end tell
    tell application "Emacs" to id of window 1
  ' 2>/dev/null) || true

  if [[ -n "$window_id" ]]; then
    screencapture -l"$window_id" -o "$output_file"
  else
    osascript -e 'tell application "Emacs" to activate'
    sleep 1
    screencapture -w -o "$output_file"
  fi
  echo "Captured: $output_file"
}

echo "=== Emacs Dashboard ==="
emacsclient --eval '(showcase--show-dashboard)'
sleep 2
capture_emacs_window "$OUTPUT_DIR/emacs-dashboard.png"

echo "=== Emacs Org Mode ==="
for variant in current focus minimal; do
  echo "=== Emacs Org Mode (${variant}) ==="
  emacsclient --eval "(showcase--show-org '${variant})"
  sleep 2
  capture_emacs_window "$OUTPUT_DIR/emacs-org-${variant}.png"
done

echo "=== Emacs Code + Treemacs ==="
emacsclient --eval '(showcase--show-code-treemacs)'
sleep 2
capture_emacs_window "$OUTPUT_DIR/emacs-code-treemacs.png"

emacsclient --eval '(delete-frame)' 2>/dev/null || true

echo "Emacs screenshots complete."
