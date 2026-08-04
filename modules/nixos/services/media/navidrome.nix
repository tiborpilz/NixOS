{ config, lib, ... }:
with lib;
with lib.my;

let
  dataDir = "/var/lib/navidrome";
  musicDir = "/data/media/music";
  publicPort = 4533;

  cfg = config.modules.services.media.navidrome;
in
{
  options.modules.services.media.navidrome = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "docker.io/deluan/navidrome:0.63.2";

    lastfmApiKey = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Optional Last.fm API key. Navidrome uses it for artist images, bios and
        similar-artist data, which is what backs the Instant Mix / radio
        feature -- without it, suggestions are limited to your own play counts.
      '';
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
            # Read-only: Lidarr owns this tree, Navidrome only indexes it.
            "${musicDir}:/music:ro"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "Europe/Berlin";
            ND_MUSICFOLDER = "/music";
            ND_DATAFOLDER = "/data";
            # Lidarr moves and renames files constantly; without this, deleted
            # paths linger in the index forever.
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
      # Navidrome has its own accounts, and the Subsonic clients that talk to it
      # cannot answer a basic-auth challenge.
      auth = false;
    };
  };
}
