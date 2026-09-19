#!/usr/bin/env

local ret_status="%(?:%{$fg_bold[green]%}▸:%{$fg_bold[red]%}▸%s)"

_vcs_branch_icon=$''
_vcs_bookmark_icon=$''

function _vcs_label() {
  local name=$1
  (( ${#name} > 35 )) && name="${name[1,35]}..."
  echo "${name//\%/%%}"
}

# First non-trunk name, so a feature branch wins over main on the same commit.
function _vcs_pick() {
  local ref
  for ref in "$@"; do
    [[ $ref == (main|master) ]] || { echo $ref; return }
  done
  [[ -n $1 ]] && echo $1
}

# Distance to the nearest bookmark below @ and that bookmark, unless it is the git ref $1.
# --ignore-working-copy keeps this read-only: otherwise jj snapshots and imports git refs as
# operations of its own, and `jj undo` later walks back into them.
function jj_prompt_info() {
  local out
  out=$(jj --ignore-working-copy log --no-graph --color=never \
    -r '@ | (latest(heads(::@ & bookmarks()))::@)' \
    -T 'local_bookmarks.map(|b| b.name()).join(" ") ++ "\t" ++ if(current_working_copy,
          if(conflict, "c") ++ if(divergent, "d") ++ "\t"
          ++ parents.map(|c| c.commit_id()).join(" ")) ++ "\n"' 2>/dev/null) || return

  local -a lines=("${(@f)out}")
  local -a head=("${(@ps:\t:)lines[1]}")
  local -a bookmarks=(${(s: :)${(@ps:\t:)lines[-1]}[1]})
  local -a parents=(${(s: :)head[3]})
  local distance=$(( ${#lines} - 1 ))
  local flags=$head[2]
  local bookmark=$(_vcs_pick ${bookmarks:#${(b)1}})
  local git_head=$(git rev-parse -q --verify HEAD 2>/dev/null)

  local -a parts=()
  (( distance )) && parts+=("+$distance")
  [[ -n $bookmark ]] && parts+=("$_vcs_bookmark_icon $(_vcs_label $bookmark)")
  [[ $flags == *c* ]] && parts+=("%F{red}⚠%F{8}")
  [[ $flags == *d* ]] && parts+=("%F{red}⇅%F{8}")
  # jj hasn't imported a git checkout or commit yet; its next command will.
  [[ -n $git_head ]] && (( ! ${parents[(Ie)$git_head]} )) && parts+=("↻")
  (( ${#parts} )) && echo "%F{8}(${(j: :)parts})%f"
}

# Runs in the worker, whose cwd is fixed at spawn time.
function _vcs_prompt_job() {
  builtin cd -q -- $1 2>/dev/null || return
  local dir=$PWD
  while [[ $dir != / && ! -d $dir/.jj ]]; do
    dir=$dir:h
  done
  local ref=$(git symbolic-ref -q --short HEAD 2>/dev/null)
  local -a parts=()
  [[ -n $ref ]] && parts+=("%F{white}$_vcs_branch_icon $(_vcs_label $ref)%f")
  local jj=
  [[ -d $dir/.jj ]] && jj=$(jj_prompt_info $ref)
  [[ -n $jj ]] && parts+=("$jj")
  (( ${#parts} )) || return
  echo " ${(j: :)parts}$(parse_git_dirty)"
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
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} ($NIX_SHELL_PACKAGES) "
    echo $nix_indicator
  elif [[ -n $IN_NIX_SHELL ]]; then
    nix_indicator="%{$fg_bold[blue]%}%{$reset_color%} "
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
  venv_indicator="%{$fg_bold[green]%}%{$reset_color%} ($venv_name) "
  echo $venv_indicator
}

typeset -g _vcs_segment= _vcs_segment_dir=

function _vcs_prompt_precmd() {
  if [[ $PWD != $_vcs_segment_dir ]]; then
    _vcs_segment=
    _vcs_segment_dir=$PWD
  fi
  async_flush_jobs _vcs_prompt_worker
  async_job _vcs_prompt_worker _vcs_prompt_job $PWD
}

function _vcs_prompt_done() {
  local job=$1 code=$2 output=$3 has_next=$6
  if [[ $job == '[async]' ]]; then
    # Worker died; restart it so the next prompt still updates.
    async_stop_worker _vcs_prompt_worker
    async_start_worker _vcs_prompt_worker
    async_register_callback _vcs_prompt_worker _vcs_prompt_done
    return
  fi
  _vcs_segment=$output
  (( has_next )) || { zle && zle reset-prompt }
}

# Re-sourcing must replace the worker, which holds the function definitions from its spawn.
async_stop_worker _vcs_prompt_worker 2>/dev/null
async_start_worker _vcs_prompt_worker
async_register_callback _vcs_prompt_worker _vcs_prompt_done
autoload -Uz add-zsh-hook
add-zsh-hook precmd _vcs_prompt_precmd

PROMPT='%{$fg_bold[white]%}$(get_pwd)%{$reset_color%} $ret_status '
RPS1='$(nix_shell_prompt)$(venv_prompt) ${_vcs_segment}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[white]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg[green]%}✓%{$reset_color%}"

#ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}[git:"
#ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
#ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}+%{$reset_color%}"
#ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"
