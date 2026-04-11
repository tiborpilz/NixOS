# FZF Git Branch Picker Showcase
# Shows fzf-git-branch() from utils.zsh with git log preview

Set Shell "zsh"
Set FontFamily "Fira Code"
Set FontSize 12
Set Padding 24
Set Theme "nord"
Set WindowBar Colorful
Set CursorBlink false
Set Width 1200
Set Height 800

# Navigate to mock repo
Type "cd /tmp/showcase"
Enter
Sleep 1s

# Call the fzf git branch picker
Type "fzf-git-branch"
Enter
Sleep 2s

# Type partial filter to show matching
Type "feat"
Sleep 2s

Screenshot "output/fzf.png"
