zstyle ':omz:alpha:lib:git' async-prompt no

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

antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
# antigen bundle docker
# antigen bundle docker-compose
# antigen bundle kubectl

# antigen bundle lukechilds/zsh-nvm

antigen apply

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Custom theme
source $ZDOTDIR/.zsh_custom/theme.zsh

# Util functions
source $ZDOTDIR/.zsh_custom/utils.zsh

# Extra config from Nix
source $ZDOTDIR/extra.zshrc

# Aliases
if type gfind >/dev/null; then alias find=gfind; fi
alias k=kubectl

# export PATH="/usr/local/bin:$PATH"

export GPG_TTY=$(tty)

# Fix xon/xoff flow control
[[ $- == *i* ]] && stty -ixon

# Env vars
## Add custom scripts etc. to Path
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.npm-global/bin
export PATH=$PATH:$HOME/go/bin

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

heck() {
  # Source THEFUCK only on demand
  if [[ -z $THEFUCK_INITIALISED ]]; then
    echo "Sourcing..."
    source <(thefuck --alias heck);
    export THEFUCK_INITIALISED=true
  fi
  heck
}

# Load Completions
if [[ $TERM != dumb  ]]; then
  autoload -Uz +X compinit
  if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
    compinit
  else
    compinit -C
  fi
  source <(kubectl completion zsh)
fi
