{ config, lib, pkgs, ... }:

{
  xsession = {
    enable = true;

    initExtra = ''
      ${pkgs.sxhkd}/bin/sxhkd &
      ${pkgs.polybar}/bin/polybar top &
    '';

    windowManager = {
      command = "${pkgs.bspwm}/bspwm &";
      bspwm = {
        extraConfig = ''
          bspc subscribe all > ~/bspc-report.log &
        '';
        startupPrograms = [ "${pkgs.polybar}/bin/polybar top &" "${pkgs.sxhkd}/sxhkd &" ];
        monitors = {
          "" = [ "web" "terminal" "III" "IV" ];
        };
        settings = {
          window_gap = 12;
        };
      };
    };
  };

  services = {
    polybar = {
      enable = true;
      script = "polybar top &";
      config = {
        "bar/top" = {
          monitor = "";
          width = "80%";
          height = 20;
          offset-x = "10%";
          offset-y = 20;
          padding = 10;
          tray-position = "right";
        };
      };
    };

    sxhkd = {
      enable = true;
      keybindings = {
        "super + b" = "${pkgs.firefox}/bin/firefox";
        "super + enter" = "xterm";
      };
      extraConfig = ''
        super + {_, shift +} {1-9, 0}
          bspc node focused --to-desktop {1-10}
      '';
    };
  };
}
