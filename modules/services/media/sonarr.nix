{ config, lib, pkgs, ... }:
with lib;

let
  sonarrConfigDir = "/var/lib/sonarr/config";
  publicPort = 8989;
in
{
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
  reverseProxy.proxies.sonarr.publicPort = publicPort;
}
