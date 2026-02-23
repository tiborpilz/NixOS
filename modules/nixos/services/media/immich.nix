{ config, lib, ... }:
with lib;
with lib.my;

let
  dataDir = "/data/media/immich";
  photoDir = "/data/media/photos";
  publicPort = 2284;
  cfg = config.modules.services.media.immich;
in
{
  options.modules.services.media.immich = {
    enable = mkBoolOpt false;
    immich-version = mkOption {
      type = types.str;
      default = "release";
    };
    db_user = mkOption {
      type = types.str;
      default = "immich";
    };
    db_password = mkOption {
      type = types.str;
      default = "immich";
    };
    db_name = mkOption {
      type = types.str;
      default = "immich";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.immich = stringAfter [ "var" ] ''
      mkdir -p ${dataDir}/upload
      mkdir -p ${photoDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) network pods;
      in
      {
        containers = {
          immich-server.containerConfig = {
            image = "ghcr.io/immich-app/immich-server:${cfg.immich-version}";
            volumes = [
              "${dataDir}/upload:/usr/src/app/upload"
              "${photoDir}:/data/media/photos"
              "/etc/localtime:/etc/localtime:ro"
            ];
            environments = {
              UPLOAD_LOCATION = "./library";
              DB_PASSWORD = cfg.db_password;
              DB_USERNAME = cfg.db_user;
              DB_DATABASE_NAME = cfg.db_name;
              IMMICH_VERSION = cfg.immich-version;
              DB_HOSTNAME = "localhost";
              REDIS_HOSTNAME = "localhost";
            };
            pod = pods.immich-pod.ref;
          };

          immich-machine-learning.containerConfig = {
            image = "ghcr.io/immich-app/immich-machine-learning:${cfg.immich-version}";
            volumes = [
              "immich-model-cache:/cache"
            ];
            pod = pods.immich-pod.ref;
          };

          immich-redis.containerConfig = {
            image = "redis:8.6";
            pod = pods.immich-pod.ref;
          };

          immich-db.containerConfig = {
            image = "tensorchord/pgvecto-rs:pg14-v0.2.0";
            environments = {
              POSTGRES_USER = cfg.db_user;
              POSTGRES_PASSWORD = cfg.db_password;
              POSTGRES_DB = cfg.db_name;
            };
            volumes = [
              "immich-pgdata:/var/lib/postgresql/data"
            ];
            pod = pods.immich-pod.ref;
          };
        };
        pods.immich-pod.podConfig = {
          publishPorts = [
            "${toString publicPort}:2283"
          ];
        };
      };

    modules.services.reverseProxy.proxies.immich = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
