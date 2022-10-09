{ options, config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  guacamolePort = 9531;
  calibrePort = 9532;
  calibreWebPort = 8083;
  calibreConfigDir = "/var/lib/calibre/config";
  calibreWebConfigDir = "/var/lib/calibre-web/config";
  cfg = config.modules.services.media.calibre;
in
{
  options.modules.services.media.calibre = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
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
    modules.services.reverseProxy.proxies.calibre.publicPort = guacamolePort;
    modules.services.reverseProxy.proxies.calibre-web.publicPort = calibreWebPort;
    modules.services.reverseProxy.proxies.calibre-web.auth = false;
  };
}
