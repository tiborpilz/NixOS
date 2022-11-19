{ inputs, config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.syncthing;
    mylib = import ../../lib { inherit inputs lib pkgs; };
in {
  options.modules.syncthing = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      tray.enable = true;
    };

    # Need to specify tray target, manually since HM is not managing the Xsession
    systemd.user.targets.tray = {
      Unit = {
        Description = "Home Manager System Tray";
        Requires = [ "graphical-session-pre.target" ];
      };
    };
  };
}
