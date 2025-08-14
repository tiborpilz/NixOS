{ config, lib, pkgs, ... }:
with lib;
with lib.my;
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
    PENPOT_FLAGS = "enable-log-emails enable-registration enable-login enable-prepl-server";
    PENPOT_PUBLIC_URI = "https://penpot.tiborpilz.xyz";
    PENPOT_DATABASE_URI = "postgresql://localhost/${db_db}";
    PENPOT_DATABASE_USERNAME = db_user;
    PENPOT_DB_PASSWORD = db_password;
    PENPOT_INITIAL_SECRET= "huddle-hypnotize-agenda-refract";
    PENPOT_REDIS_URI = "redis://localhost:${toString redisPort}/0";
    PENPOT_ASSETS_STORAGE_BACKEND = "fs";
    PENPOT_STORAGE_FS_DIRECTORY = "/assets";
    PENPOT_REGISTRATION_DOMAIN_WHITELIST = "pilz.berlin,olynet.de";
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
    publicAdminPort = mkOption {
      type = types.int;
      default = 9002;
      description = "Port to expose Penpot web UI on.";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.initPenpot = stringAfter [ "var" ] ''
      mkdir -p ${penpotDbDir}
      mkdir -p ${penpotAssetsDir}
    '';
    virtualisation.quadlet = 
    let
      inherit (config.virtualisation.quadlet) network pods;
    in {
      containers = {
        penpot-backend.containerConfig = {
          image = "penpotapp/backend:2.7.1";
          environments = penpotEnv;
          volumes = [
            "${penpotAssetsDir}:/assets"
          ];
          pod = pods.${podName}.ref;
        };

        penpot-frontend.containerConfig = {
          image = "penpotapp/frontend:2.7.1";
          environments = penpotEnv;
          pod = pods.${podName}.ref;
        };

        penpot-exporter.containerConfig = {
          image = "penpotapp/exporter:2.7.1";
          environments = penpotEnv;
          pod = pods.${podName}.ref;
        };

        penpot-redis.containerConfig = {
          image = "redis:7";
          pod = pods.${podName}.ref;
        };

        penpot-db.containerConfig = {
          image = "postgres:17";
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
            "${toString penpotPort}:8080"
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
