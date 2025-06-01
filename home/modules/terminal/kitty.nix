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
      settings = {
        enable_audio_bell = false;
      };
      extraConfig = ''
        window_margin_width 32
        macos_titlebar_color background
        wayland_titlebar_color background
        macos_traditional_fullscreen yes

        background_opacity 0.7
        background_blur 0.5

        forward_stdio no
      '';
    };
  };
}
