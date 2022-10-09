{ config, lib, pkgs, ... }:
with lib;

let
  guacamolePort = 9531;
  calibrePort = 9532;
  calibreWebPort = 8083;
  calibreConfigDir = "/var/lib/calibre/config";
  calibreWebConfigDir = "/var/lib/calibre-web/config";
in
{
  system.activationScripts.makeCalibreDir = stringAfter [ "var" ] ''
    mkdir -p ${calibreConfigDir}
    mkdir -p ${calibreWebConfigDir}
  '';

  virtualisation.oci-containers.containers.calibre = {
    image = "lscr.io/linuxserver/calibre:latest";
    ports = [
      "${toString guacamolePort}:8080"
      "${toString calibrePort}:8081"
    ];
    volumes = [
      "${calibreConfigDir}:/config"
      "/data/media/books:/books"
    ];
    environment = {
      "TZ" = "Europe/Berlin";
      "PUID" = "0";
      "PGID" = "0";
    };
  };

  virtualisation.oci-containers.containers.calibre-web = {
    image = "lscr.io/linuxserver/calibre-web:latest";
    ports = [
      "${toString calibreWebPort}:8083"
    ];
    volumes = [
      "${calibreWebConfigDir}:/config"
      "/data/media/books:/books"
    ];
    environment = {
      "TZ" = "Europe/Berlin";
      "PUID" = "0";
      "PGID" = "0";
      # "DOCKER_MODS" = "linuxserver/mods:universal-calibre";
      "OAUTHLIB_RELAX_TOKEN_SCOPE" = "1";
    };
  };
  services.reverseProxy.proxies.calibre.publicPort = guacamolePort;
  services.reverseProxy.proxies.calibre-web.publicPort = calibreWebPort;
  services.reverseProxy.proxies.calibre-web.auth = false;
}
