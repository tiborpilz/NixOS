{ config, pkgs, lib, ... }:

let
  cfg = config.modules.terminal.kitty;
in
with lib;
{
  options.modules.terminal.kitty = {
    enable = mkEnableOption "Kitty terminal emulator";
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        package = pkgs.fira-code;
        size = 14;
        name = "Fira Code";
      };
      themeFile = "Nord";
      settings = {
        enable_audio_bell = false;
      };
      extraConfig = ''
        window_margin_width 16
      '';
    };
  };
}
