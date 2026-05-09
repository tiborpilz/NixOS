# Profiling: always on. Each shell appends a zprof report to $ZPROF_LOG
# (default ~/.cache/zsh/zprof.log) with a header identifying the parent
# process, so tmux-spawned vs nested shells can be compared.
#   tail -100 ~/.cache/zsh/zprof.log    # most recent runs
#   : > ~/.cache/zsh/zprof.log          # truncate
zmodload zsh/zprof

# Make nix pkgs take precedence over system binaries
export PATH=$HOME/.nix-profile/bin:$PATH

# Zsh Async Library
source $ZDOTDIR/.zsh_custom/async.zsh

# Zsh Options
setopt prompt_subst

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

# Custom theme
source $ZDOTDIR/.zsh_custom/theme.zsh

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Util functions
source $ZDOTDIR/.zsh_custom/utils.zsh

# Aliases
if type gfind >/dev/null; then alias find=gfind; fi

# export PATH="/usr/local/bin:$PATH"

export GPG_TTY=$(tty)

# Fix xon/xoff flow control
[[ $- == *i* ]] && stty -ixon

# Env vars
## Add custom scripts etc. to Path
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.npm-global/bin
export PATH=$PATH:$HOME/go/bin

# Make nix pkgs take precedence over system binaries
export PATH=$HOME/.nix-profile/bin:$PATH

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
export NIX_PATH=/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

# Load Completions
# Rebuild the compdump at most once a day; otherwise skip the security audit
# and reuse the cached dump. The `(#q...)` glob qualifier needs extendedglob,
# which we enable locally inside the anonymous function.
if [[ $TERM != dumb ]]; then
  autoload -Uz compinit
  () {
    setopt local_options extendedglob
    local _zdump=${ZDOTDIR:-$HOME}/.zcompdump
    if [[ ! -e $_zdump || -n ${_zdump}(#qN.mh+24) ]]; then
      compinit
    else
      compinit -C
    fi
  }
fi

# Initialize zoxide
# TODO: Once we get rid of the non-home-assistant zshrc, stuff like this should be also taken care of.
source <(zoxide init zsh)

# Android stuff
# TODO move to nix
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools

# Extra config from Nix
source $ZDOTDIR/extra.zshrc

{
  ZPROF_LOG=${ZPROF_LOG:-${ZSH_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/zsh}/zprof.log}
  mkdir -p ${ZPROF_LOG:h} 2>/dev/null
  # Always append to the log; if ZSH_PROFILE is set, also echo to the terminal.
  {
    print -- "=== $(date '+%Y-%m-%d %H:%M:%S') SHLVL=$SHLVL pid=$$ ppid=$PPID parent=$(ps -o comm= -p $PPID 2>/dev/null) login=$([[ -o login ]] && print yes || print no) tty=${TTY:-?} term=$TERM_PROGRAM ==="
    zprof
    print
  } | tee -a $ZPROF_LOG ${ZSH_PROFILE:+/dev/stderr} >/dev/null
}
