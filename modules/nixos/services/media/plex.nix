{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  plexConfigDir = "/var/lib/plex/config";
  cfg = config.modules.services.media.plex;
in
{
  options.modules.services.media.plex = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
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
      extraOptions = [ "--network=host" ];
    };
  };
}
