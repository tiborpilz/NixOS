{ inputs, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.shell.tmux;
  # Monkey-Patch tmux to use XDG_CONFIG_HOME
  tmux = (pkgs.writeScriptBin "tmux" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.tmux}/bin/tmux -f "$TMUX_HOME/tmux.conf" "$@"
  '');
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.tmux = with types; {
    enable = mylib.mkBoolOpt false;
    rcFiles = mylib.mkOpt (listOf (either str path)) [ ];
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.gitmux
    ];
    programs.tmux = {
      enable = true;
      plugins = with pkgs; [
        tmuxPlugins.copycat
        tmuxPlugins.yank
        tmuxPlugins.prefix-highlight
        tmuxPlugins.net-speed
        tmuxPlugins.cpu
        tmuxPlugins.sidebar
        tmuxPlugins.tmux-fzf
      ];

      baseIndex = 1;
      escapeTime = 0;
      keyMode = "vi";

      extraConfig = ''
        # Add Nix PATH
        set-environment -g 'PATH' "$HOME/.nix-profile/bin:$PATH"

        # Sometimes I wanna use the mouse, ok?
        set -g mouse on

        # Vim-ish key bindings
        bind P paste-buffer
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi y send-keys -X copy-selection
        bind-key -T copy-mode-vi r send-keys -X rectangle-toggle

        # Actually copy to clipboard (on Linux)
        bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel 'xclip -in -selection clipboard'

        # Attach current directory to session
        bind a attach -c "#{pane_current_path}"

        # Use tmux-fzf to switch sessions
        bind-key S run-shell -b "${pkgs.tmuxPlugins.tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/session.sh switch"

        # Use tmux-fzf to switch windows
        bind-key W run-shell -b "${pkgs.tmuxPlugins.tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/window.sh switch"

        # Reload tmux config with prefix + r
        bind-key r source-file $XDG_CONFIG_HOME/tmux/tmux.conf

        # Fix duplicated Characters in zsh
        # (https://stackoverflow.com/questions/45931164/duplicated-characters-and-non-updating-input-using-tmux-in-zsh)

        # Misc Settings
        set -g monitor-activity off
        set -g visual-activity on
        set -g status-interval 1
        set -g focus-events on
        set -g renumber-windows on

        # Status Bar
        set -g status-position top
        set -g status-justify left
        set -g status-style bg=default,fg=colour4

        set -g window-status-format '| ○ #W '
        set -g window-status-current-style 'bg=default,fg=colour7'
        set -g window-status-current-format '| ● #W '


        set -g pane-border-format ""
        set -g pane-border-status top
        set -g pane-border-lines single
        set -g pane-border-style 'fg=colour0'

        # Set active pane border to dark gay, same as pane-border
        set -g pane-active-border-style 'fg=colour0'

        # Only show active pane border when there's more than one pane
        is_many="if [ #{window_panes} -eq 1 ]; then exit 1; fi"

        set-hook -g window-layout-changed 'if-shell "$is_many" "set-option -w pane-active-border-style fg=colour4" "set-option -w pane-active-border-style fg=colour0'
        set-hook -g session-window-changed 'if-shell "$is_many" "set-option -w pane-active-border-style fg=colour4" "set-option -w pane-active-border-style fg=colour0'

        # TODO: these don't quite work yet
        set-hook -g session-window-changed 'run-shell refresh_tmux_starship'
        set-hook -g after-rename-window 'run-shell refresh_tmux_starship'
        set-hook -g after-select-pane 'run-shell refresh_tmux_starship'
        set-hook -g after-select-window 'run-shell refresh_tmux_starship'

        set -g status-left-length 40
        set -g status-left '#[fg=colour4]  #S #[default]'

        set -g status-right-length 120

        status_starship="#(${pkgs.starship}/bin/starship  prompt | head -n2 | tail -n1 | xargs ${pkgs.my.ansi2tmux}/bin/ansi2tmux)"
        set -g status-right "$status_starship"

        # Normally, only show the current window name and the time
        # set -g status-right #[fg=colour4] #(date +"%H:%M") | #S '

        # show_git="false"

        # status_git="#(${pkgs.gitmux}/bin/gitmux -cfg $HOME/.config/gitmux/gitmux.conf #{pane_current_path}) #[fg=colour4] | #(date +%H:%M)"
        # status_no_git="#[fg=colour4] #(date +%H:%M)"

        # # TODO: get toggle working
        # # set -g status-right "#[if:#{==:#{@show_git],true}]$status_git#[else]$status_no_git#[endif]"
        # set -g status-right "$status_git "
      '';
    };

    xdg.configFile."gitmux/gitmux.conf".text = ''
      tmux:
        styles:
          clear: '#[fg=colour4]'
          state: '#[fg=colour4]'
          branch: '#[fg=colour7]'
          remote: '#[fg=colour4]'
          divergence: '#[fg=colour4]'
          staged: '#[fg=colour6]'
          conflict: '#[fg=colour4]'
          modified: '#[fg=colour4]'
          untracked: '#[fg=colour4]'
          stashed: '#[fg=colour4]'
          clean: '#[fg=colour4]'
          insertions: '#[fg=colour4]'
          deletions: '#[fg=colour4]'
        layout: [branch, ' ', divergence, '- ', flags]
    '';

    # I'm only using starship as a panel for tmux, hence the config is here
    programs.starship = {
      enable = true;
      settings = {
        shell = {
          disabled = true;
        };
        aws = {
          disabled = true;
        };
        gcloud = {
          disabled = true;
        };
        directory = {
          disabled = true;
        };
        git_branch = {
          format = "[$symbol$branch(:$remote_branch) ]($style)";
          truncation_length = 12;
        };
        git_status = {
          format = "[$modified$up_to_date ]($style)";
          up_to_date = "✓";
          modified = "✗";
          stashed = "";
        };
        nix_shell = {
          symbol = "❄️";
          format = "[$symbol$name]($style)";
        };
      };
    };

    modules.shell.zsh.rcInit = ''
        autoload -U add-zsh-hook

        get_raw_starship_prompt() {
          ${pkgs.starship}/bin/starship prompt | head -n2 | tail -n1
        }

        refresh_tmux_starship() {
          # Early return if not TMUX
          if [[ -z "$TMUX" ]]; then
            return
          fi

          delay=''${1:-0}
          sleep $delay
          tmux set -g status-right "$(get_raw_starship_prompt | xargs ${pkgs.my.ansi2tmux}/bin/ansi2tmux))"
        }

        export PERIOD=1
        add-zsh-hook chpwd refresh_tmux_starship
        add-zsh-hook precmd refresh_tmux_starship
        add-zsh-hook periodic refresh_tmux_starship
    '';

    home.sessionVariables = {
      TMUX_HOME = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmux";
      TMUXIFIER = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmuxifier";
      TMUXIFIER_LAYOUT_PATH = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmuxifier";
      TMUX_FZF_OPTIONS = "-p -w 75% -h 66% -m";
    };
  };
}
