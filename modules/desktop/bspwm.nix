{ options, config, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.bspwm;
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
      picom.enable = true;
      # redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
          lightdm.greeters.mini.user = "tibor";
        };
        windowManager.bspwm.enable = true;
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    home-manager.users.tibor.xdg.configFile = {
      "sxhkd".source = ../../home/config/sxhkd;
      "bspwm" = {
        source = ../../home/config/bspwm;
        recursive = true;
      };
    };
  };
}
