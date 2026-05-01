# Neovim Dashboard Showcase
# Shows alpha-nvim dashboard with braille logo and quick buttons
#
# TODO: VHS Screenshot does not produce a PNG for this tape — likely the
# alpha-nvim dashboard isn't fully painted at the screenshot moment.
# Currently skipped in CI via SCREENSHOTS_SKIP=neovim-dashboard. See
# .github/workflows/screenshots.yml.

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 800

# Open neovim with no args to show dashboard
Type "cd /tmp/showcase && nvim"
Enter

# Wait for lazy.nvim to finish loading
Sleep 8s

Screenshot "output/neovim-dashboard.png"
