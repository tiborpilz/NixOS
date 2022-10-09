{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  sonarrConfigDir = "/var/lib/sonarr/config";
  publicPort = 8989;
  cfg = config.modules.services.media.sonarr;
in
{
  options.modules.services.media.sonarr = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeSonarrDir = stringAfter [ "var" ] ''
      mkdir -p ${sonarrConfigDir}
    '';

    virtualisation.oci-containers.containers.sonarr = {
      image = "lscr.io/linuxserver/sonarr:latest";
      ports = [ "${toString publicPort}:8989" ];
      volumes = [
        "${sonarrConfigDir}:/config"
        "/data/media/tv:/tv"
        "/data/downloads:/downloads"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
        "PUID" = "0";
        "PGID" = "0";
      };
    };
    services.reverseProxy.proxies.sonarr.publicPort = publicPort;
  };
}
