{ config, lib, pkgs, ... }:
with lib;

let
  plexConfigDir = "/var/lib/plex/config";
in
  {
    system.activationScripts.makePlexDir = stringAfter [ "var" ] ''
      mkdir -p ${plexConfigDir}
    '';

    virtualisation.oci-containers.containers.plex = {
      image = "lscr.io/linuxserver/plex:latest";
      volumes = [
        "${plexConfigDir}:/config"
        "/data/media/tv:/tv"
        "/data/media/movies:/movies"
        ];
        environment = {
        "PUID" = "0";
        "PGID" = "0";
        "VERSION" = "docker";
      };
      extraOptions = ["--network=host"];
    };
  }
