#!/usr/bin/env bash
set -euo pipefail

# Run all termshot screenshot scenes. Each scene is a self-contained script
# under screenshots/scripts/termshot-scenes/<name>.sh that captures a command's
# colored output and renders a framed PNG via termshot - no X server required,
# unlike the Kitty scenes in run-scenes.sh.
#
# These cover the *command-output* showcases (git history, diffs,
# syntax-highlighted source). Interactive TUIs stay in run-scenes.sh.
#
# SCREENSHOTS_SKIP=name1,name2 to skip specific scenes.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCENES_DIR="${SCRIPT_DIR}/termshot-scenes"

# The git/bat scenes read the mock repo; make sure it exists before we start.
SHOWCASE_DIR="${SHOWCASE_DIR:-/tmp/showcase}"
if [[ ! -d "$SHOWCASE_DIR/.git" ]]; then
  echo "Mock repo missing at ${SHOWCASE_DIR}; creating it..."
  bash "${SCRIPT_DIR}/mock-git-repo.sh"
fi

SCENES=(
  "git-graph"
  "git-diff"
  "bat"
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

for scene in "${SCENES[@]}"; do
  if is_skipped "$scene"; then
    echo ""
    echo "--- ${scene} (skipped via SCREENSHOTS_SKIP) ---"
    continue
  fi
  scene_script="${SCENES_DIR}/${scene}.sh"
  if [[ ! -f "$scene_script" ]]; then
    echo "WARNING: scene script not found: $scene_script"
    continue
  fi
  echo ""
  echo "--- ${scene} ---"
  bash "$scene_script"
  echo "  Done: ${scene}"
done
