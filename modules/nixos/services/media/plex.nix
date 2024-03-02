{ config, lib, ... }:
with lib;
with lib.my;

let
  plexConfigDir = "/var/lib/plex/config";
  cfg = config.modules.services.media.plex;
in
{
  options.modules.services.media.plex = {
    enable = mkBoolOpt false;
    allowedNetworks = mkOption {
      type = types.str;
      description = "CIDR of the allowed network";
      default = "";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.makePlexDir = stringAfter [ "var" ] ''
      mkdir -p ${plexConfigDir}
    '';

    modules.services.reverseProxy.proxies.plex.publicPort = 32400;

    virtualisation.oci-containers.containers.plex = {
      image = "plexinc/pms-docker:latest";
      volumes = [
        "${plexConfigDir}:/config"
        "/data/media/tv:/tv"
        "/data/media/movies:/movies"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
      } // optionalAttrs (cfg.allowedNetworks != "") {
        "ALLOWED_NETWORKS" = cfg.allowedNetworks;
      };
      extraOptions = [ "--network=host" ];
    };
  };
}
