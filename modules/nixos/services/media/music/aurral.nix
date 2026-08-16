{ config, lib, ... }:
with lib;
with lib.my;

let
  configDir = "/var/lib/aurral/config";
  musicDir = music.libraryDir;
  downloadsDir = "${music.downloadsDir}/aurral";
  slskdDownloadsDir = "${music.downloadsDir}/slskd";
  publicPort = 3001;

  music = config.modules.services.media.music;
  cfg = music.aurral;
in
{
  options.modules.services.media.music.aurral = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "ghcr.io/lklynet/aurral:2.0.3";

    contactEmail = mkOption {
      type = types.str;
      description = "Address sent in the MusicBrainz API User-Agent header.";
    };

    envFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Optional sops-encrypted env file. Recognised keys: LIDARR_API_KEY,
        LASTFM_API_KEY.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.aurral = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
      mkdir -p ${downloadsDir}
    '';

    virtualisation.quadlet.containers.aurral = {
      containerConfig = {
        image = cfg.image;
        networks = [ "host" ];
        volumes = [
          "${configDir}:/config:rw"
          "${musicDir}:${musicDir}:ro"
          "${downloadsDir}:${downloadsDir}:rw"
          "${slskdDownloadsDir}:${slskdDownloadsDir}:rw"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environments = {
          TZ = "Europe/Berlin";
          PUID = "0";
          PGID = "0";
          PORT = toString publicPort;
          LIDARR_URL = "http://localhost:8686";
          CONTACT_EMAIL = cfg.contactEmail;
          DOWNLOAD_FOLDER = downloadsDir;
        };
        environmentFiles = optional (cfg.envFile != null) cfg.envFile;
      };
      unitConfig.After = [ "lidarr.service" "slskd.service" ];
    };

    modules.services.reverseProxy.proxies.aurral = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
