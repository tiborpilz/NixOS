{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  jackettConfigDir = "/var/lib/jackett/config";
  publicPort = 9117;
  cfg = config.modules.services.media.jackett;
in
{
  options.modules.services.media.jackett = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makejackettDir = stringAfter [ "var" ] ''
      mkdir -p ${jackettConfigDir}
    '';

    virtualisation.oci-containers.containers.jackett = {
      image = "lscr.io/linuxserver/jackett:latest";
      ports = [ "${toString publicPort}:9117" ];
      volumes = [
        "${jackettConfigDir}:/config"
        "/data/media/tv:/tv"
        "/data/downloads:/downloads"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
      };
    };
    modules.services.reverseProxy.proxies.jackett.publicPort = publicPort;
  };
}
