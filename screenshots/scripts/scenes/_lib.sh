#!/usr/bin/env bash
# Common helpers for kitty-based screenshot scenes. Sourced by sibling
# scripts. Each scene script:
#   . _lib.sh
#   launch_kitty "command-to-run-in-shell"
#   sleep N; type_keys "..."; press Return; ...
#   capture "$OUTPUT_DIR/<name>.png"

# -E (errtrace) is essential: without it the ERR trap below is NOT inherited by
# shell functions, so an error inside frame()/capture()/launch_kitty() would
# abort the scene via `set -e` with zero diagnostics (a silent exit 1).
set -Eeuo pipefail

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
KITTY_W=""
KITTY_H=""
KITTY_CONF=""

# Resolve the kitty config the scenes must render with. We always want the
# repo's config (Nord theme, Fira Code, window margins), never kitty's built-in
# defaults. setup-ci-configs.sh symlinks the home-manager-generated kitty.conf
# to ~/.config/kitty/kitty.conf (CI); on the owner's machine that path already
# is the live home-manager config. $KITTY_CONFIG overrides for tests/ad-hoc runs.
resolve_kitty_conf() {
  local conf="${KITTY_CONFIG:-$HOME/.config/kitty/kitty.conf}"
  if [[ ! -s "$conf" ]]; then
    echo "ERROR: kitty config not found or empty: $conf" >&2
    echo "       Screenshots must render with the repo's kitty config, not kitty defaults." >&2
    echo "       Run screenshots/scripts/setup-ci-configs.sh first (CI does), or point" >&2
    echo "       \$KITTY_CONFIG at a kitty.conf." >&2
    return 1
  fi
  printf '%s' "$conf"
}

launch_kitty() {
  local cmd="${1:-zsh -i}"
  local width="${2:-1200}"
  local height="${3:-800}"
  KITTY_W="$width"
  KITTY_H="$height"

  # Pin kitty to the repo's config explicitly instead of relying on default
  # discovery. If ~/.config/kitty/kitty.conf is missing - or XDG_CONFIG_HOME
  # points somewhere else - default discovery silently falls back to kitty's
  # built-in theme/font; --config plus the existence check make that fail loudly.
  KITTY_CONF="$(resolve_kitty_conf)" || return 1

  kitty \
    --config "$KITTY_CONF" \
    --override "remember_window_size=no" \
    --override "initial_window_width=${width}" \
    --override "initial_window_height=${height}" \
    --override "cursor_blink_interval=0" \
    -e bash -c "$cmd" >/tmp/kitty.log 2>&1 &
  KITTY_PID=$!

  local deadline=$((SECONDS + 15))
  while (( SECONDS < deadline )); do
    # Gather *every* visible kitty window. A leaked instance from a prior scene
    # would let `tail -1` silently grab the wrong window, so detect >1 and fail.
    local wids count
    wids=$(xdotool search --onlyvisible --class kitty 2>/dev/null || true)
    if [[ -n "$wids" ]]; then
      count=$(printf '%s\n' "$wids" | grep -c .)
      if (( count > 1 )); then
        echo "ERROR: expected exactly one kitty window, found ${count}:" >&2
        printf '  wid=%s\n' $wids >&2
        echo "--- kitty log ---" >&2
        cat /tmp/kitty.log >&2 || true
        return 1
      fi
      KITTY_WID="$wids"
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
  echo "--- scene failed (exit ${exit_code}) at ${BASH_SOURCE[1]:-?}:${BASH_LINENO[0]:-?}: ${BASH_COMMAND}" >&2
  echo "--- kitty log ---" >&2
  cat /tmp/kitty.log >&2 2>/dev/null || true
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

# Nord palette for the window chrome (matches the kitty Nord theme).
NORD_BG="#2E3440"        # nord0  - content/window background
NORD_TITLEBAR="#3B4252"  # nord1  - titlebar
NORD_FG="#D8DEE9"        # nord4  - title text + button icons
NORD_CLOSE="#BF616A"     # nord11 - close button accent

# Best-effort: resolve the Fira Code file for the titlebar title text. If
# fontconfig can't find it we just omit the text (the buttons still render).
TITLE_FONT="$(fc-match -f '%{file}' 'FiraCode Nerd Font Mono' 2>/dev/null || true)"

# Wrap a raw terminal grab in a KDE/Breeze-style window: a Nord titlebar with
# minimise/maximise/close buttons on the right, interior padding, rounded
# corners, a soft drop shadow, and a transparent margin - so it reads like a
# real window floating on the desktop.
frame() {
  local in="$1" out="$2" title="${3:-}"
  local pad=18 tbh=40 radius=10 cw ch
  # `magick identify -format` emits no trailing newline, so `read … < <(…)`
  # would hit EOF without a delimiter and return non-zero, aborting the scene
  # under `set -e`. A here-string appends the newline, so the read succeeds.
  read -r cw ch <<<"$(magick identify -format '%w %h' "$in")"
  if [[ -z "$cw" || -z "$ch" ]]; then
    echo "ERROR: frame: could not read dimensions of grab '$in'" >&2
    return 1
  fi

  local content bar
  content="$(mktemp --suffix=.png)"
  bar="$(mktemp --suffix=.png)"

  # Interior padding (window background showing around the terminal content).
  magick "$in" -background "$NORD_BG" \
    -gravity West -splice "${pad}x0" \
    -gravity East -splice "${pad}x0" \
    -gravity South -splice "0x${pad}" \
    "$content"

  local pw=$((cw + 2 * pad))
  local fh=$((tbh + ch + pad))

  # Titlebar + Breeze-style window buttons (minimise | maximise | close).
  local cy=$((tbh / 2)) r=6
  local x_close=$((pw - 22)) x_max x_min
  x_max=$((x_close - 26))
  x_min=$((x_max - 26))
  magick -size "${pw}x${tbh}" "xc:${NORD_TITLEBAR}" \
    -fill none -strokewidth 2 \
    -stroke "$NORD_FG" \
    -draw "line $((x_min - r)),$((cy + r)) $((x_min + r)),$((cy + r))" \
    -draw "rectangle $((x_max - r)),$((cy - r)) $((x_max + r)),$((cy + r))" \
    -stroke "$NORD_CLOSE" \
    -draw "line $((x_close - r)),$((cy - r)) $((x_close + r)),$((cy + r))" \
    -draw "line $((x_close - r)),$((cy + r)) $((x_close + r)),$((cy - r))" \
    "$bar"

  if [[ -n "$TITLE_FONT" && -n "$title" ]]; then
    magick "$bar" -font "$TITLE_FONT" -pointsize 15 -fill "$NORD_FG" \
      -gravity West -annotate +18+0 "$title" "$bar"
  fi

  # Stack titlebar over content, round the corners, add shadow + margin.
  magick \( "$bar" "$content" -append \) \
    \( -size "${pw}x${fh}" xc:none -fill white \
       -draw "roundrectangle 0,0 $((pw - 1)),$((fh - 1)) ${radius},${radius}" \) \
    -compose DstIn -composite \
    -compose Over \
    \( +clone -background black -shadow 55x18+0+14 \) \
    +swap -background none -layers merge +repage \
    -bordercolor none -border 40 \
    "$out"

  rm -f "$content" "$bar"
}

# Verify the grab will hit a *live* kitty window, not a crashed/stale one. Run
# right before `import` so we fail loudly instead of capturing whatever X left
# behind. Uses only xdotool (xprop isn't in the dev shell).
assert_kitty_live() {
  if [[ -z "$KITTY_PID" ]] || ! kill -0 "$KITTY_PID" 2>/dev/null; then
    echo "ERROR: kitty process (pid=${KITTY_PID:-none}) is not running at capture time" >&2
    return 1
  fi
  if [[ -z "$KITTY_WID" ]]; then
    echo "ERROR: no kitty window id recorded at capture time" >&2
    return 1
  fi
  # Re-confirm the recorded id is still a visible window of class kitty.
  if ! xdotool search --onlyvisible --class kitty 2>/dev/null | grep -qx "$KITTY_WID"; then
    echo "ERROR: window ${KITTY_WID} is no longer a visible kitty window at capture time" >&2
    return 1
  fi
}

# Sanity-check a fresh grab before we frame it. Catches two silent failures:
#   * a degenerate grab (1x1 icon, destroyed window) - guarded by a size floor;
#   * an all-one-color frame (kitty up but nothing rendered, or an empty window)
#     - %k is the unique-color count, and real anti-aliased text has many shades.
assert_sane_grab() {
  local img="$1" w h colors
  read -r w h <<<"$(magick identify -format '%w %h' "$img" 2>/dev/null || true)"
  if [[ -z "$w" || -z "$h" ]]; then
    echo "ERROR: could not read dimensions of grab '$img'" >&2
    return 1
  fi
  if (( w < 200 || h < 200 )); then
    echo "ERROR: grab '$img' is ${w}x${h}, too small to be a real terminal" >&2
    return 1
  fi
  colors=$(magick identify -format '%k' "$img" 2>/dev/null || echo 0)
  if ! [[ "$colors" =~ ^[0-9]+$ ]] || (( colors < 16 )); then
    echo "ERROR: grab '$img' has ${colors:-0} unique color(s) - looks blank" >&2
    return 1
  fi
}

capture() {
  local out="$1" title="${2:-}" raw
  assert_kitty_live
  raw="$(mktemp --suffix=.png)"
  import -window "$KITTY_WID" "$raw"
  assert_sane_grab "$raw"
  frame "$raw" "$out" "$title"
  rm -f "$raw"
  echo "Captured: $out"
}
