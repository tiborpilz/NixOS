{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  readarrConfigDir = "/var/lib/readarr/config";
  publicPort = 8787;
  cfg = config.modules.services.media.readarr;
in
{
  options.modules.services.media.readarr = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makereadarrDir = stringAfter [ "var" ] ''
      mkdir -p ${readarrConfigDir}
    '';

    virtualisation.oci-containers.containers.readarr = {
      image = "docker.io/linuxserver/readarr:0.1.1-nightly";
      ports = [ "${toString publicPort}:8787" ];
      volumes = [
        "${readarrConfigDir}:/config"
        "/data/media/books:/books"
        "/data/downloads:/downloads"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
        "PUID" = "0";
        "PGID" = "0";
      };
    };
    modules.services.reverseProxy.proxies.readarr.publicPort = publicPort;
  };
}
