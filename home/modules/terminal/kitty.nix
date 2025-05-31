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
        allow_remote_control = true;
        dynamic_background_opacity = true;
      };
      extraConfig = ''
        dynamic_background_opacity yes
        allow_remote_control yes
        listen_on yes
        window_margin_width 32

        macos_titlebar_color background
        wayland_titlebar_color background
        macos_traditional_fullscreen yes
      '';
    };
  };
}
