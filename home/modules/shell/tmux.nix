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
    programs.tmux = {
      enable = true;
      plugins = with pkgs; [
        tmuxPlugins.copycat
        tmuxPlugins.yank
        tmuxPlugins.prefix-highlight
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

        # Attach current directory to session
        bind a attach -c "#{pane_current_path}"

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
        set -g status-justify centre
        set -g status-bg colour8
        set -g status-fg white

        set -g window-status-format ' #W '
        set -g window-status-current-style 'bg=default,fg=colour4'
        set -g window-status-current-format ' #W '

        set -g status-right-length 40
        set -g status-right '#(${pkgs.gitmux}/bin/gitmux "#{pane_current_path}")'
      '';
    };

    home.sessionVariables = {
      TMUX_HOME = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmux";
      TMUXIFIER = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmuxifier";
      TMUXIFIER_LAYOUT_PATH = "${config.home.sessionVariables.XDG_CONFIG_HOME}/tmuxifier";
    };
  };
}
