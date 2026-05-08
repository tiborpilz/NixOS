#!/usr/bin/env bash
# Run plenary busted tests for the neovim config.
# Requires plenary.nvim and nui.nvim to be installed via lazy.nvim.

set -euo pipefail

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$dir"

exec nvim --headless --noplugin -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"
