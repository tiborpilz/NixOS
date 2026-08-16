{ config, lib, ... }:
with lib;
with lib.my;

let
  dataDir = "/var/lib/navidrome";
  musicDir = music.libraryDir;
  publicPort = 4533;

  music = config.modules.services.media.music;
  cfg = music.navidrome;
in
{
  options.modules.services.media.music.navidrome = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "docker.io/deluan/navidrome:0.63.2";

    lastfmApiKey = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Optional Last.fm API key.";
    };

    lastfmSecret = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Matching Last.fm shared secret.";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.navidrome = stringAfter [ "var" ] ''
      mkdir -p ${dataDir}
      mkdir -p ${musicDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        containers.navidrome.containerConfig = {
          image = cfg.image;
          volumes = [
            "${dataDir}:/data:rw"
            "${musicDir}:/music:ro"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "Europe/Berlin";
            ND_MUSICFOLDER = "/music";
            ND_DATAFOLDER = "/data";
            # Lidarr moves files constantly; without this, deleted paths
            # linger in the index forever.
            ND_SCANNER_PURGEMISSING = "always";
            ND_LOGLEVEL = "info";
          } // optionalAttrs (cfg.lastfmApiKey != null) {
            ND_LASTFM_ENABLED = "true";
            ND_LASTFM_APIKEY = cfg.lastfmApiKey;
          } // optionalAttrs (cfg.lastfmSecret != null) {
            ND_LASTFM_SECRET = cfg.lastfmSecret;
          };
          pod = pods.navidrome-pod.ref;
        };

        pods.navidrome-pod.podConfig = {
          publishPorts = [
            "${toString publicPort}:4533"
          ];
        };
      };

    modules.services.reverseProxy.proxies.navidrome = {
      publicPort = publicPort;
      # Subsonic clients cannot answer a basic-auth challenge.
      auth = false;
    };
  };
}
