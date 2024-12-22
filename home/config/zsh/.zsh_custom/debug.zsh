# # Define the custom widget
# function change_prompt() {
#   PROMPT="%F{blue}%n@%m%f %F{green}%~%f > "
#   RPROMPT="%F{yellow}[%?]%f"
#   zle reset-prompt  # Redraw the prompt
# }

# # Bind the widget to a key (e.g., Ctrl-G)
# zle -N change_promptP
# bindkey "^G" change_prompt

setopt PROMPT_SUBST
PROMPT='%B%F{red}%n@%m%f%F{yellow}[%D{%L:%M:%S}]%f:%F{blue}${${(%):-%~}}%f$ %b'
TMOUT=1

TRAPALRM() {
    zle reset-prompt
}
