{ config, lib, ... }:
with lib;
with lib.my;

let
  configDir = "/var/lib/aurral/config";
  musicDir = "/data/media/music";
  publicPort = 3001;

  cfg = config.modules.services.media.aurral;
in
{
  options.modules.services.media.aurral = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "ghcr.io/lklynet/aurral:2.0.3";

    contactEmail = mkOption {
      type = types.str;
      description = ''
        Address sent in the MusicBrainz API User-Agent header. MusicBrainz
        rate-limits (and eventually blocks) anonymous clients, so this is not
        optional in practice.
      '';
    };

    envFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Optional sops-encrypted env file for LIDARR_API_KEY and, if you want
        artist artwork, LASTFM_API_KEY. Aurral 2.x can also be configured
        entirely from its web UI, in which case this can stay null.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.aurral = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
    '';

    virtualisation.quadlet.containers.aurral = {
      containerConfig = {
        image = cfg.image;
        # Host networking so Aurral can reach Lidarr on its published port.
        networks = [ "host" ];
        volumes = [
          "${configDir}:/config:rw"
          "${musicDir}:${musicDir}:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environments = {
          TZ = "Europe/Berlin";
          PUID = "0";
          PGID = "0";
          PORT = toString publicPort;
          LIDARR_URL = "http://localhost:8686";
          CONTACT_EMAIL = cfg.contactEmail;
        };
        environmentFiles = optional (cfg.envFile != null) cfg.envFile;
      };
      unitConfig.After = [ "lidarr.service" ];
    };

    modules.services.reverseProxy.proxies.aurral = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
