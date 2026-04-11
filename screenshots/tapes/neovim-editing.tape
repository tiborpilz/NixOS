# Neovim Editing Showcase
# Shows neovim editing a Nix file with Nord theme, lualine, neo-tree sidebar

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 800

# Open neovim on the sample Nix file
Type "cd /tmp/showcase && nvim config.nix"
Enter

# Wait for plugins to load
Sleep 6s

# Toggle neo-tree sidebar with <Space>op
Type " op"
Sleep 2s

Screenshot "output/neovim-editing.png"
