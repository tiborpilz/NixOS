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
        package = pkgs.nerd-fonts.fira-code;
        size = 14;
        name = "FiraCode Nerd Font Mono";
      };
      themeFile = "Nord";
      settings = {
        enable_audio_bell = false;
      };
      extraConfig = ''
        window_margin_width 24
        macos_titlebar_color background
      '';
    };
  };
}
