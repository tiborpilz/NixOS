### Simple transient prompt

[[ -c /dev/null ]]  ||  return
zmodload zsh/system ||  return


ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}✓%{$reset_color%}"

local ret_status="%(?:%{$fg_bold[green]%}▸:%{$fg_bold[red]%}▸%s)"

# Show the current git branch and whether it is dirty
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX  $(current_branch | sed 's/\(.\{35\}\).*/\1.../')$ZSH_THEME_GIT_PROMPT_SUFFIX$(parse_git_dirty)"
}

# If in the root of a git repo, show only the repo name, otherwise show the path
function get_pwd(){
  git_root=$PWD
  while [[ $git_root != / && ! -e $git_root/.git ]]; do
    git_root=$git_root:h
  done
  if [[ $git_root = / ]]; then
    unset git_root
    prompt_short_dir=%~
  else
    parent=${git_root%\/*}
    prompt_short_dir=${PWD#$parent/}
  fi
  echo $prompt_short_dir
}

# Check whether we are in a nix shell
function nix_shell_prompt() {
  if [[ -n $NIX_SHELL_PACKAGES ]]; then
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} ($NIX_SHELL_PACKAGES) "
    echo $nix_indicator
  elif [[ -n $IN_NIX_SHELL ]]; then
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} "
    echo $nix_indicator
  fi
}

# Check whether we are in a virtualenv
function venv_prompt() {
  if [[ -z $VIRTUAL_ENV ]]; then
    return
  fi
  venv_name=$(basename $(dirname $VIRTUAL_ENV))
  venv_indicator="%{$fg_bold[green]%}%{$reset_color%} ($venv_name) "
  echo $venv_indicator
}


TRANSIENT_PROMPT='%# '

function set_prompt {
    PROMPT='%{$fg_bold[white]%}$(get_pwd)%{$reset_color%} $ret_status '
    RPS1='$(nix_shell_prompt)$(venv_prompt) $(git_prompt_info)'
}

zle -N send-break _transient_prompt_widget-send-break
function _transient_prompt_widget-send-break {
    _transient_prompt_widget-zle-line-finish
    zle .send-break
}

zle -N zle-line-finish _transient_prompt_widget-zle-line-finish
function _transient_prompt_widget-zle-line-finish {
    (( ! _transient_prompt_fd )) && {
        sysopen -r -o cloexec -u _transient_prompt_fd /dev/null
        zle -F $_transient_prompt_fd _transient_prompt_restore_prompt
    }
    zle && PROMPT='%{$fg_bold[white]%}$(get_pwd)%{$reset_color%} $ret_status ' RPROMPT= zle reset-prompt && zle -R
}

function _transient_prompt_restore_prompt {
    exec {1}>&-
    (( ${+1} )) && zle -F $1
    _transient_prompt_fd=0
    set_prompt
    zle reset-prompt
    zle -R
}

(( ${+precmd_functions} )) || typeset -ga precmd_functions
(( ${#precmd_functions} )) || {
    do_nothing() {true}
    precmd_functions=(do_nothing)
}

precmd_functions+=_transient_prompt_precmd
function _transient_prompt_precmd {
    TRAPINT() { zle && _transient_prompt_widget-zle-line-finish; return $(( 128 + $1 )) }
}


# vim: sw=0 ts=4 sts=4 et
