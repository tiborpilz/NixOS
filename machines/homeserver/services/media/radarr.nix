{ config, lib, pkgs, ... }:
with lib;

let
  radarrConfigDir = "/var/lib/radarr/config";
in
  {
    system.activationScripts.makeradarrDir = stringAfter [ "var" ] ''
      mkdir -p ${radarrConfigDir}
    '';

    virtualisation.oci-containers.containers.radarr = {
      image = "lscr.io/linuxserver/radarr:latest";
      ports = ["7878:7878"];
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
  }
