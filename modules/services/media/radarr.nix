{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  radarrConfigDir = "/var/lib/radarr/config";
  publicPort = 7878;
  cfg = config.modules.services.media.radarr;
in
{
  options.modules.services.media.radarr = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeradarrDir = stringAfter [ "var" ] ''
      mkdir -p ${radarrConfigDir}
    '';

    virtualisation.oci-containers.containers.radarr = {
      image = "lscr.io/linuxserver/radarr:latest";
      ports = [ "${toString publicPort}:7878" ];
      volumes = [
        "${radarrConfigDir}:/config"
        "/data/media/movies:/movies"
        "/data/downloads:/downloads"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
        "PUID" = "0";
        "PGID" = "0";
      };
    };
    services.reverseProxy.proxies.radarr.publicPort = publicPort;
  };
}
