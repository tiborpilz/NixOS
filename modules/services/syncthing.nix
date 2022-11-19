{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.syncthing;
  port = 8384;
in
{
  options.modules.services.syncthing = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    syncthing = {
      enable = true;
      dataDir = "/data/syncthing";
      configDir = "/var/lib/syncthing";
      guiAddress = "0.0.0.0:${port}";
    };

    networking.firewall.allowedTCPPorts = [ port 22000 ];
    networking.firewall.allowedUDPPorts = [ 22000 21027 ];
    modules.services.reverseProxy.proxies.syncthing.publicPort = port;
  };
}
