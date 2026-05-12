#!/usr/bin/env bash
# Common helpers for kitty-based screenshot scenes. Sourced by sibling
# scripts. Each scene script:
#   . _lib.sh
#   launch_kitty "command-to-run-in-shell"
#   sleep N; type_keys "..."; press Return; ...
#   capture "$OUTPUT_DIR/<name>.png"

set -euo pipefail

SCENE_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS_DIR="$(cd "${SCENE_LIB_DIR}/.." && pwd)"
SCREENSHOTS_DIR="$(cd "${SCRIPTS_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${SCREENSHOTS_DIR}/.." && pwd)"
OUTPUT_DIR="${SCREENSHOTS_DIR}/output"

mkdir -p "$OUTPUT_DIR"

if [[ -z "${DISPLAY:-}" ]]; then
  echo "ERROR: DISPLAY not set. Run inside xvfb-run or start Xvfb." >&2
  exit 1
fi

# Xvfb has no GPU; force Mesa software rasterizer for Kitty's GL needs.
export LIBGL_ALWAYS_SOFTWARE=1

KITTY_PID=""
KITTY_WID=""

launch_kitty() {
  local cmd="${1:-zsh -i}"
  local width="${2:-1200}"
  local height="${3:-800}"

  # Kitty picks up ~/.config/kitty/kitty.conf by default; setup-ci-configs.sh
  # symlinks it from the home-manager-generated config.
  kitty \
    --override "remember_window_size=no" \
    --override "initial_window_width=${width}" \
    --override "initial_window_height=${height}" \
    --override "cursor_blink_interval=0" \
    -e bash -c "$cmd" >/tmp/kitty.log 2>&1 &
  KITTY_PID=$!

  local deadline=$((SECONDS + 15))
  while (( SECONDS < deadline )); do
    KITTY_WID=$(xdotool search --onlyvisible --class kitty 2>/dev/null | tail -1 || true)
    if [[ -n "$KITTY_WID" ]]; then
      # No WM under Xvfb: windowactivate is a no-op, windowfocus sets X focus directly.
      xdotool windowfocus --sync "$KITTY_WID" 2>/dev/null || true
      return 0
    fi
    sleep 0.25
  done

  echo "ERROR: kitty window did not appear within timeout" >&2
  echo "--- kitty log ---" >&2
  cat /tmp/kitty.log >&2 || true
  return 1
}

dump_kitty_log_on_error() {
  local exit_code=$?
  if (( exit_code != 0 )); then
    echo "--- kitty log (scene failed with exit $exit_code) ---" >&2
    cat /tmp/kitty.log >&2 || true
  fi
}
trap dump_kitty_log_on_error ERR

cleanup_kitty() {
  if [[ -n "$KITTY_PID" ]]; then
    kill "$KITTY_PID" 2>/dev/null || true
    wait "$KITTY_PID" 2>/dev/null || true
    KITTY_PID=""
  fi
}

trap cleanup_kitty EXIT

type_keys() {
  # Refocus before each batch; XSendEvent (--window) gets rejected by kitty,
  # so we rely on XTestFakeKeyEvent into the focused window instead.
  xdotool windowfocus --sync "$KITTY_WID" 2>/dev/null || true
  xdotool type --delay 25 -- "$@"
}

press() {
  xdotool windowfocus --sync "$KITTY_WID" 2>/dev/null || true
  for key in "$@"; do
    xdotool key -- "$key"
  done
}

capture() {
  local out="$1"
  import -window "$KITTY_WID" "$out"
  echo "Captured: $out"
}
