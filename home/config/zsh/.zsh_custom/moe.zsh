function prompt_moe_setup () {
    emulate -L zsh
    autoload -Uz vcs_info
    autoload -Uz zsh/sched

    # Check if we are the root user
    local user='%B%F{blue}%n%f%b'
    local remote=""
    if [[ $UID -eq 0 ]]; then
        user='%B%F{red}%n%f%b'
    fi

    if [[ -n $SSH_CLIENT ]]; then
        remote="%Sremote ⚛%s"
    fi

    local dir='%~'
    local good='(っ・ω・%)っ'
    local bad='%B%F{red}(ﾒ ﾟ皿ﾟ%)ﾒ%f%b'
    ps1_stub="${user}@%m %B%~%b"
    ps1_upper=$ps1_stub
    ps1_lower="%(?.$good .$bad )"

    PS1="${ps1_upper}$prompt_newline${ps1_lower}"
    RPS1="%(?..%F{red}\$?%f) ${remote}"

    # Customized vcs_info format, See: http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Version-Control-Information

    # Enable vcs check for git unstaged changes, this is will decrease the performace in huge repo
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' unstagedstr ' %B%F{red}+%f%b'

    # Set vcs_info action formats (means when we are in rebase / merge progress)
    zstyle ':vcs_info:*' actionformats '%F{magenta}(%f%s%F{magenta})%F{yellow}-%F{magenta}[%F{green}%b%F{yellow}|%F{red}%a%u%F{magenta}]%f' 'zsh %r'

    # Set vcs info formats (normal vcs format) 
    zstyle ':vcs_info:*' formats '- %F{magenta}[%F{green}%b%u%F{magenta}]%f ' 'zsh %r'

    # Use async git prompt, speed up the prompt in huge git repo
    async_start_worker vcs_updater_worker
    async_register_callback vcs_updater_worker do_update

    add-zsh-hook precmd prompt_moe_precmd
}

function do_update() {
    vcs_info
    ps1_upper="$ps1_stub \$vcs_info_msg_0_"
    PS1="${ps1_upper}$prompt_newline${ps1_lower}"
    zle reset-prompt
}

function prompt_moe_precmd() {
    # We set default upper prompt without vcs
    # asynchronously update it in do_update
    # ps1_upper=$ps1_stub
    # PS1="${ps1_upper}$prompt_newline${ps1_lower}"
    if [ -d .git ] || git rev-parse --git-dir > /dev/null 2> /dev/null; then
        async_job vcs_updater_worker
    else
        ps1_upper=$ps1_stub
        PS1="${ps1_upper}$prompt_newline${ps1_lower}"
    fi
}

function prompt_moe_help() {
    echo "HELP"
}

# enable substitution and treat percent as special characters
# http://zsh.sourceforge.net/Doc/Release/Options.html
prompt_opts=(subst percent cr)
prompt_moe_setup "$@"
