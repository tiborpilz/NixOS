{ config, lib, ... }:

let
  cfg = config.modules.services.bitmagnet;
in
{
  options.modules.services.bitmagnet = {
    enable = lib.mkEnableOption "BitMagnet Service";

    publicApiPort = lib.mkOption {
      type = lib.types.int;
      default = 3333;
      description = "Public API and WebUI port.";
    };

    publicTorrentPort = lib.mkOption {
      type = lib.types.int;
      default = 3334;
      description = "Public BitTorrent port.";
    };

    dataDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/bitmagnet";
      description = "Directory for storing persistent data.";
    };

    tmdbApiKey = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "TMDB API key.";
    };

    runDhtCrawler = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable or disable DHT crawler.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Ensure the data directory exists
    system.activationScripts.bitmagnet = lib.stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) network pods;
      in
      {
        containers = {
          bitmagnet-db.containerConfig = {
            image = "postgres:16-alpine";
            volumes = [
              "${cfg.dataDir}/postgres:/var/lib/postgresql/data"
            ];
            environments = {
              POSTGRES_PASSWORD = "postgres";
              POSTGRES_DB = "bitmagnet";
              PGUSER = "postgres";
            };
            extraOptions = [ "--shm-size=1g" ]; # translate shm_size to extraOptions
            pod = pods.bitmagnet-pod.ref;
          };

          bitmagnet.containerConfig = {
            image = "ghcr.io/bitmagnet-io/bitmagnet:latest";
            restartPolicy = "unless-stopped";
            volumes = [
              "${cfg.dataDir}/data:/data"
            ];
            environments = {
              POSTGRES_HOST = "postgres";
              POSTGRES_PASSWORD = "postgres";
              TMDB_API_KEY = cfg.tmdbApiKey or "";
            };
            command = [ "worker" "run" "--keys=http_server" "--keys=queue_server" ]
              ++ lib.optional cfg.runDhtCrawler "--keys=dht_crawler";
            pod = pods.bitmagnet-pod.ref;
          };
        };
        pods.bitmagnet-pod.podConfig = {
          publishPorts = [
            "${toString cfg.publicApiPort}:3333"
            "${toString cfg.publicTorrentPort}:3334/tcp"
            "${toString cfg.publicTorrentPort}:3334/udp"
          ];
        };
      };

    modules.services.reverseProxy.proxies.bitmagnet = {
      publicPort = cfg.publicApiPort;
      auth = true;
    };
  };
}
