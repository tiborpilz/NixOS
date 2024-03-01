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
      port = "${toString publicPort}:3001";

      containers.immich-server = {
        image = "ghcr.io/immich-app/immich-server:${cfg.immich-version}";
        cmd = [ "start-server.sh" ];
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
          "DB_HOSTNAME" = "localhost";
          "REDIS_HOSTNAME" = "localhost";
        };
      };

      containers.immich-microservices = {
        image = "ghcr.io/immich-app/immich-server:${cfg.immich-version}";
        cmd = [ "start.sh" "microservices" ];
        volumes = [
          "${dataDir}/upload:/usr/src/app/upload"
          "/data/media/photos:/data/media/photos"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environment = {
          "UPLOAD_LOCATION" = "./library";
          "DB_PASSWORD" = cfg.db_password;
          "DB_USERNAME" = cfg.db_user;
          "DB_DATABASE_NAME" = cfg.db_name;
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
        image = "redis:6.2-alpine@sha256:b6124ab2e45cc332e16398022a411d7e37181f21ff7874835e0180f56a09e82a";
      };

      containers.db = {
        image = "tensorchord/pgvecto-rs:pg14-v0.1.11@sha256:0335a1a22f8c5dd1b697f14f079934f5152eaaa216c09b61e293be285491f8ee";
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
