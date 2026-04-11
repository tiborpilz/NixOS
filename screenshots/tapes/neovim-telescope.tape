# Neovim Telescope Showcase
# Shows telescope fuzzy finder with vertical layout and backdrop dimming

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 800

# Open neovim in the mock repo
Type "cd /tmp/showcase && nvim ."
Enter

# Wait for plugins to load
Sleep 6s

# Open telescope frecency finder with <Space>pf
Type " pf"
Sleep 2s

# Type a partial query to show results
Type "config"
Sleep 2s

Screenshot "output/neovim-telescope.png"
