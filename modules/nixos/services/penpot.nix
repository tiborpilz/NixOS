{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.services.penpot;
  penpotDataDir = cfg.dataDir;
  penpotDbDir = "${penpotDataDir}/postgres";
  penpotAssetsDir = "${penpotDataDir}/assets";
  penpotPort = cfg.publicPort;
  db_user = "penpot";
  db_password = "penpot";
  db_db = "penpot";
  podName = "penpot-pod";
  dbPort = 5432;
  redisPort = 6379;
  penpotEnv = {
    PENPOT_FLAGS = "enable-registration enable-login enable-prepl-server";
    PENPOT_PUBLIC_URI = "http://localhost:${toString penpotPort}";
    PENPOT_DATABASE_URI = "postgresql://${db_user}:${db_password}@localhost:${toString dbPort}/${db_db}";
    PENPOT_REDIS_URI = "redis://localhost:${toString redisPort}/0";
    PENPOT_ASSETS_STORAGE_BACKEND = "fs";
    PENPOT_STORAGE_FS_DIRECTORY = "/assets";
  };
  dbEnv = {
    POSTGRES_USER = db_user;
    POSTGRES_PASSWORD = db_password;
    POSTGRES_DB = db_db;
  };
in
{
  options.modules.services.penpot = {
    enable = mkBoolOpt false;
    dataDir = mkOption {
      type = types.str;
      default = "/data/penpot";
      description = "Base directory for Penpot persistent data (assets, db).";
    };
    publicPort = mkOption {
      type = types.int;
      default = 9001;
      description = "Port to expose Penpot web UI on.";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.initPenpot = stringAfter [ "var" ] ''
      mkdir -p ${penpotDbDir}
      mkdir -p ${penpotAssetsDir}
    '';
    virtualisation.quadlet = {
      containers = {
        "penpot-backend".containerConfig = {
          image = "penpot-backend:latest";
          environments = penpotEnv;
          volumes = [
            "${penpotAssetsDir}:/assets"
          ];
          pod = pods.${podName}.ref;
          dependsOn = [ "penpot-db" "penpot-redis" ];
        };
        "penpot-frontend".containerConfig = {
          image = "penpot-frontend:latest";
          environments = penpotEnv;
          pod = pods.${podName}.ref;
          dependsOn = [ "penpot-backend" "penpot-exporter" ];
        };
        "penpot-exporter".containerConfig = {
          image = "penpot-exporter:latest";
          environments = penpotEnv;
          pod = pods.${podName}.ref;
          dependsOn = [ "penpot-backend" ];
        };
        "penpot-redis".containerConfig = {
          image = "redis:7-alpine";
          pod = pods.${podName}.ref;
        };
        "penpot-db".containerConfig = {
          image = "postgres:15-alpine";
          environments = dbEnv;
          volumes = [
            "${penpotDbDir}:/var/lib/postgresql/data"
          ];
          pod = pods.${podName}.ref;
        };
      };
      pods = {
        "${podName}".podConfig = {
          publishPorts = [
            "${toString penpotPort}:9001"
          ];
        };
      };
    };
    modules.services.reverseProxy.proxies.penpot = {
      publicPort = penpotPort;
      auth = false;
    };
  };
} 