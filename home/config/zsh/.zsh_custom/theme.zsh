#!/usr/bin/env

local ret_status="%(?:%{$fg_bold[green]%}▸:%{$fg_bold[red]%}▸%s)"

# Jujutsu Prompt
# TODO: Move somewhere else

autoload -Uz vcs_info
autoload -U colors && colors

zstyle ':vcs_info:*' enable jj git

precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst

zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
+vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null ; then
        hook_com[staged]+='!' # signify new files with a bang
    fi
}

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*:*' formats " %{$fg[blue]%}(%s %{$fg[red]%}%m%u%{$fg[white]%}%{$fg[blue]%} %b%{$fg[blue]%})%{$reset_color%}"

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX  $(current_branch | sed 's/\(.\{35\}\).*/\1.../')$ZSH_THEME_GIT_PROMPT_SUFFIX$(parse_git_dirty)"
}

function jj_prompt() {
  echo "\${vcs_info_msg_0_}"
}

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

function nix_shell_prompt() {
  if [[ -n $NIX_SHELL_PACKAGES ]]; then
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} ($NIX_SHELL_PACKAGES) "
    echo $nix_indicator
  elif [[ -n $IN_NIX_SHELL ]]; then
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} "
    echo $nix_indicator
  fi
}

# We use our own venv prompt, so we can disable the default
export VIRTUAL_ENV_DISABLE_PROMPT=1
function venv_prompt() {
  if [[ -z $VIRTUAL_ENV ]]; then
    return
  fi
  venv_name=$(basename $(dirname $VIRTUAL_ENV))
  venv_indicator="%{$fg_bold[green]%}%{$reset_color%} ($venv_name) "
  echo $venv_indicator
}

PROMPT='%{$fg_bold[white]%}$(get_pwd)%{$reset_color%} $ret_status '
RPS1="$(nix_shell_prompt)$(venv_prompt) $(git_prompt_info) $(jj_prompt)"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}✓%{$reset_color%}"
