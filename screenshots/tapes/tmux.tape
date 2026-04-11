# Tmux Showcase
# Shows top status bar, session name, window indicators, pane splits, gitmux

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 800

# Start a new tmux session
Type "cd /tmp/showcase && tmux new-session -s nixos -n Editor"
Enter
Sleep 2s

# Open neovim in the first window
Type "nvim config.nix"
Enter
Sleep 3s

# Create a second window named Shell
Type@100ms "\x01"
Type "c"
Sleep 500ms

# Rename the window
Type@100ms "\x01"
Type ","
Sleep 500ms
Type "Shell"
Enter
Sleep 500ms

# Run a command in the Shell window
Type "cd /tmp/showcase && git log --oneline -5"
Enter
Sleep 1s

# Switch back to the Editor window
Type@100ms "\x01"
Type "1"
Sleep 1s

# Create a horizontal split
Type@100ms "\x01"
Type '"'
Sleep 1s

# Run something in the bottom pane
Type "git diff --stat"
Enter
Sleep 1s

# Switch to the top pane
Type@100ms "\x01"
Type "k"
Sleep 1s

Screenshot "output/tmux.png"
