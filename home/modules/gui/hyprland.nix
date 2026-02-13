{ inputs, lib, pkgs, config, options, ... }:

with lib;
let
  cfg = config.modules.gui.hyprland;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  mod = "SUPER";
in
{
  options.modules.gui.hyprland.enable = mylib.mkBoolOpt true;

  config = mkIf cfg.enable {
    # Let HM manage user-side Wayland bits
    home.packages = with pkgs; [
      kitty
      waybar
      mako
      wofi
      grim
      slurp
      wl-clipboard
      swaylock
      hyprpaper
    ];

    services.mako.enable = true;

    programs.waybar.enable = true;

    # Hyprland config via HM
    wayland.windowManager.hyprland = {
      enable = true;

      settings = {
        monitor = [ ",preferred,auto,1" ];

        exec-once = [
          "hyprpaper"
          "waybar"
          "mako"
        ];

        input = {
          kb_layout = "us";
          follow_mouse = 1;
          touchpad = { natural_scroll = true; };
        };

        general = {
          gaps_in = 5;
          gaps_out = 10;
          border_size = 2;
          layout = "dwindle";
        };

        decoration = {
          rounding = 8;
          blur = { enabled = true; size = 8; passes = 2; };
        };

        animations = { enabled = true; };

        "$mod" = mod;

        bind = [
          "$mod, RETURN, exec, kitty"
          "$mod, D, exec, wofi --show drun"
          "$mod, Q, killactive"
          "$mod, M, exit"
          "$mod, V, togglefloating"
          "$mod, F, fullscreen"
          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"

          # Screenshot to clipboard (select area)
          ", Print, exec, grim -g \"$(slurp)\" - | wl-copy"
        ];

        # Mouse move/resize
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
      };
    };
  }
};
