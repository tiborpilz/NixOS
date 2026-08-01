{ config, options, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.tailTray;
  mylib = import ../../lib { inherit inputs pkgs lib; };
in
{
  options.modules.tailTray = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.tail-tray ];

    # tail-tray ships a .desktop entry but nothing that autostarts it, so run it
    # as part of the graphical session. It only ever draws a tray icon, hence
    # graphical-session.target - it needs the panel's system tray to exist.
    # Talking to tailscaled as a non-root user requires the operator to be set;
    # that's services.tailscale.extraSetFlags on the NixOS side.
    systemd.user.services.tail-tray = {
      Unit = {
        Description = "Tail Tray (Tailscale tray icon)";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.tail-tray}/bin/tail-tray";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
