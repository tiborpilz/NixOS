# Add custom scripts to Path
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.npm-global/bin
PATH=$PATH:$HOME/go/bin


# Antigen Plugin Manager
if [[ ! -a $HOME/.antigen/antigen.zsh ]]; then
  git clone --branch master https://github.com/zsh-users/antigen.git ~/.antigen
  cd ~/.antigen && git checkout v2.2.3 && cd ~
fi

source $HOME/.antigen/antigen.zsh

# Fix slow nvm startup time
export NVM_LAZY_LOAD=true

antigen use oh-my-zsh

antigen bundle vi-mode

antigen bundle wfxr/forgit
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting

antigen bundle lukechilds/zsh-nvm

antigen apply

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Custom theme
source $ZDOTDIR/.zsh_custom/themes/lnclt.zsh-theme

# Fix xon/xoff flow control
[[ $- == *i* ]] && stty -ixon

# Functions
python-init() {
  # Init python venv in current dir if no argument given
  projectPath=${PWD##*/}

  if [ -n "$1" ]
  then
    echo "$1"
    export project="$1"
    mkdir "$1"
  else
    export project=${PWD##*/}
  fi

  export projectPath="./${project}"

  # Create .venv folder with project folder name as prompt
  python -m venv "$projectPath/.venv" --prompt "$project"
  source "${projectPath}/.venv/bin/activate"

  # Activate venv and install kernelspec for jupyter
  pip install ipykernel
  python -m ipykernel install --user --name "$project"
  pip install jupyter
}

# Select Git Branch interactively
fzf-git-branch() {
  git rev-parse HEAD > /dev/null 2>&1 || return # not a git repo

  git branch --color=always --all --sort=-committerdate |
    grep -v HEAD |
    fzf-tmux --ansi --no-multi --preview-window right:65% \
      --preview 'git log -n 50 --color=always --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed "s/.* //" <<< {})' \
      | sed "s/.* //"
}

# Interactive git checkout
gch() {
  git checkout $(fzf-git-branch)
}

# Interactive npm run
npr() {
  command=$(jq -r '.scripts | keys | .[]' package.json | fzf-tmux --preview "jq -r '.scripts[\"{}\"]' package.json")
  if [[ ! -z command ]]; then
    print -z "npm run $command"
  fi
}

# Interactive cd to repo in ~/Code
ccd() {
  cd $(find ~/Code/* -type d -maxdepth 0 | fzf-tmux --preview 'if [ -f {}/README.md ]; then glow {}/README.md --style=dark; else ls {}; fi')
}

source <(kubectl completion zsh)
alias k=kubectl

export PATH="/usr/local/opt/mysql-client/bin:$PATH"
export PATH="/usr/local/opt/python@3.8/bin:$PATH"
export PATH="/usr/local/bin:$PATH"

export GPG_TTY=$(tty)

if type gfind >/dev/null; then alias find=gfind; fi

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

if [[ $TERM != dumb  ]]; then
  autoload -Uz compinit && compinit -u -d $ZSH_CACHE/zcompdump
  source $ZDOTDIR/extra.zshrc
fi
