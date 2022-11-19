{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.bspwm;
in {
  options.modules.desktop.bspwm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
    ];
    services = {
      # picom.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          sddm.enable = true;
          # lightdm.greeters.mini.enable = true;
        };
        windowManager.bspwm.enable = true;
      };
    };

    home.configFile = {
      # "sxhkd" = {
      #   source = ../../home/config/sxhkd;
      #   recursive = true;
      # };
      "bspwm" = {
        source = ../../home/config/bspwm;
        recursive = true;
      };
    };

    home-manager.users.tibor.services.polybar = {
      enable = true;
      script = "polybar top &";
      config = {
        "bar/top" = {
          monitor = "";
          width = "100%";
          height = 20;
          # offset-y = 10;
          # padding = 10;
          tray-position = "right";
          modules-right = "date";
          modules-left = "bspwm";
        };
        "module/date" = {
          type = "internal/date";
          internal = 5;
          date = "%d.%m.%y";
          time = "%H:%M";
          label = "%time% %date%";
        };
        "module/bspwm" = {
          type = "internal/bspwm";
          pin-workspaces = true;
          enable-click = true;
          fuzzy-match = true;
          # ws-icon-default = "♟";
        };
      };
    };

    home-manager.users.tibor.services.sxhkd = {
      enable = true;
      keybindings = {
        "super + b" = "${pkgs.firefox}/bin/firefox";
        "super + Return" = "${pkgs.xterm}/bin/xterm";
        "super + e" = "${pkgs.emacs}/bin/emacs";
        "super + space" = "${pkgs.rofi}/bin/rofi -show drun";
      };
      extraConfig = ''
          super + {1-5}
            bspc desktop -f {1-5}
        '';
    };

    home-manager.users.tibor.programs.rofi = {
      enable = true;
      pass = {
        enable = true;
      };
      plugins = [
        pkgs.rofi-calc
      ];
    };
    services.picom = {
      enable = true;
      fade = true;
      fadeDelta = 1;
      fadeSteps = [ 0.01 0.012 ];
      shadow = true;
      shadowOffsets = [ (-10) (-10) ];
      shadowOpacity = 0.22;

      settings = {
        shadow-radius = 12;
      };
    };
    # xsession = {
    #   enable = true;

    #   initExtra = ''
    #     ${pkgs.sxhkd}/bin/sxhkd &
    #     ${pkgs.polybar}/bin/polybar top &
    #   '';

    #   windowManager = {
    #     command = "${pkgs.bspwm}/bspwm &";
    #     bspwm = {
    #       extraConfig = ''
    #         bspc subscribe all > ~/bspc-report.log &
    #       '';
    #       startupPrograms = [ "${pkgs.polybar}/bin" ];
    #     };
    #   };
    # };
    # services.xserver.enable = true;
    # services.xserver.displayManager.sddm.enable = true;
    # services.xserver.displayManager.session = "bspwm";
  };
}
