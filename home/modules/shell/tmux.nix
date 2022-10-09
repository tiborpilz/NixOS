{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.tmux;
    # Monkey-Patch tmux to use XDG_CONFIG_HOME
    tmux = (pkgs.writeScriptBin "tmux" ''
      #!${pkgs.stdenv.shell}
      exec ${pkgs.tmux}/bin/tmux -f "$TMUX_HOME/config" "$@"
    '');
in {
  options.modules.shell.tmux = with types; {
    enable = mkBoolOpt false;
    rcFiles = mkOpt (listOf (either str path)) [];
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ tmux ];

    modules.shell.zsh = {
      rcInit = "_cache tmuxifier init -";
    };

    xdg.configFile = {
      "tmux" = { source = ../../config/tmux; recursive = true; };
      "tmux/extraInit".text = ''
        # This file is auto-generated by nixos, don't edit by hand!
        tmux run-shell ${pkgs.tmuxPlugins.copycat}/share/tmux-plugins/copycat/copycat.tmux
        tmux run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
        tmux run-shell ${pkgs.tmuxPlugins.prefix-highlight}/share/tmux-plugins/yank/yank.tmux
        tmux run-shell '${pkgs.tmuxPlugins.prefix-highlight}/share/tmux-plugins/prefix-highlight/prefix_highlight.tmux'
        ${concatMapStrings (path: "source '${path}'\n") cfg.rcFiles}
      '';
    };

    home.sessionPath = [ "$TMUXIFIER/bin" ];
    home.sessionVariables = {
      TMUX_HOME = "$XDG_CONFIG_HOME/tmux";
      TMUXIFIER = "$XDG_DATA_HOME/tmuxifier";
      TMUXIFIER_LAYOUT_PATH = "$XDG_DATA_HOME/tmuxifier";
    };
  };
}
