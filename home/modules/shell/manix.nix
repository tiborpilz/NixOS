{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.shell.manix;
in
{
  options.modules.shell.manix = {
    enable = mkEnableOption "manix";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.manix
    ];

    modules.shell.zsh.rcInit = ''
      manix-fzf() {
        manix "" \
          | grep '^# ' \
          | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' \
          | ${pkgs.fzf}/bin/fzf --preview="manix '{}' | ${pkgs.glow}/bin/glow --style=dark" \
          | xargs manix
      }
    '';
  };
}
