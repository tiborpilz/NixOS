{ config, lib, ... }:
with lib;
with lib.my;

let
  dataDir = "/data/media/immich";
  photoDir = "/data/media/photos";
  publicPort = 2283;
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

    modules.podgroups.pods.immich = {
      port = "${toString publicPort}:2283";

      containers.immich-server = {
        image = "ghcr.io/immich-app/immich-server:${cfg.immich-version}";
        volumes = [
          "${dataDir}/upload:/usr/src/app/upload"
          "${photoDir}:/data/media/photos"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environment = {
          "UPLOAD_LOCATION" = "./library";
          "DB_PASSWORD" = cfg.db_password;
          "DB_USERNAME" = cfg.db_user;
          "DB_DATABASE_NAME" = cfg.db_name;
          "IMMICH_VERSION" = cfg.immich-version;
          "DB_HOSTNAME" = "localhost";
          "REDIS_HOSTNAME" = "localhost";
        };
      };

      containers.immich-machine-learning = {
        image = "ghcr.io/immich-app/immich-machine-learning:${cfg.immich-version}";
        volumes = [
          "immich-model-cache:/cache"
        ];
        environment = {
          # TODO
        };
      };

      containers.redis = {
        image = "redis:7.4";
      };

      containers.db = {
        image = "tensorchord/pgvecto-rs:pg14-v0.2.0";
        environment = {
          POSTGRES_USER = cfg.db_user;
          POSTGRES_PASSWORD = cfg.db_password;
          POSTGRES_DB = cfg.db_name;
          # TODO
        };
        volumes = [
          "immich-pgdata:/var/lib/postgresql/data"
        ];
      };
    };

    modules.services.reverseProxy.proxies.immich = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
