# ZSH Prompt Showcase
# Shows custom theme.zsh prompt: directory, ▸ arrow, git branch/status, nix-shell indicator

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 400

# Navigate to mock repo to trigger git prompt
Type "cd /tmp/showcase"
Enter
Sleep 1s

# Show basic prompt with git info
Type "git status --short"
Enter
Sleep 1s

# Show directory listing
Type "ls"
Enter
Sleep 1s

# Show nix-shell indicator
Type "IN_NIX_SHELL=1 exec zsh"
Enter
Sleep 1.5s

Type "echo 'nix-shell active'"
Enter
Sleep 1s

Screenshot "output/zsh-prompt.png"
