#!/usr/bin/env bash

fzf-git-branch() {
  git rev-parse HEAD > /dev/null 2>&1 || return # not a git repo

  git reflog --date=iso \
    | grep 'checkout: moving from' \
    | awk '{print $NF}' \
    | awk '!seen[$0]++' \
    | fzf --ansi --no-multi --preview-window right:65% \
      --preview 'git log -n 50 --color=always --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed "s/.* //" <<< {})' \
    | sed "s/.* //"
}

# Interactive git checkout
gch() {
  git checkout $(fzf-git-branch)
}

gch
