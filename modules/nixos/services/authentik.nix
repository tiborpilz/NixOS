{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.authentik;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # Database configuration
  db_user = "authentik";
  db_password = "authentik";
  db_db = "authentik";

  # Redis configuration
  redis_password = "authentik-redis-password";
in
with mylib;
{
  options.modules.services.authentik = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 9000;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/authentik";
    };
    envFile = mkOption {
      type = types.str;
      description = "SOPS-encrypted environment file containing AUTHENTIK_SECRET_KEY and other sensitive vars";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.quadlet =
      let inherit (config.virtualisation.quadlet) network pods; in
      {
        containers = {
          authentik-db.containerConfig = {
            image = "postgres:16-alpine";
            volumes = [
              "authentik-pgdata:/var/lib/postgresql/data"
            ];
            environments = {
              POSTGRES_USER = db_user;
              POSTGRES_PASSWORD = db_password;
              POSTGRES_DB = db_db;
            };
            pod = pods.authentik-pod.ref;
          };

          authentik-cache.containerConfig = {
            image = "redis:alpine";
            exec = [
              "--save"
              "60"
              "1"
              "--loglevel"
              "warning"
              "--requirepass"
              redis_password
            ];
            volumes = [
              "authentik-redis:/data"
            ];
            pod = pods.authentik-pod.ref;
          };

          authentik-server.containerConfig = {
            image = "ghcr.io/goauthentik/server:2024.12";
            exec = "server";
            volumes = [
              "${cfg.dataDir}/media:/media"
              "${cfg.dataDir}/custom-templates:/templates"
            ];
            environments = {
              AUTHENTIK_REDIS__HOST = "localhost";
              AUTHENTIK_REDIS__PASSWORD = redis_password;
              AUTHENTIK_POSTGRESQL__HOST = "localhost";
              AUTHENTIK_POSTGRESQL__USER = db_user;
              AUTHENTIK_POSTGRESQL__PASSWORD = db_password;
              AUTHENTIK_POSTGRESQL__NAME = db_db;
              AUTHENTIK_DEFAULT_USER_CHANGE_EMAIL = "false";
              AUTHENTIK_DEFAULT_USER_CHANGE_NAME = "false";
              AUTHENTIK_DEFAULT_USER_CHANGE_USERNAME = "false";
              AUTHENTIK_DISABLE_STARTUP_ANALYTICS = "true";
              AUTHENTIK_DISABLE_UPDATE_CHECK = "true";
              AUTHENTIK_AUTHENTIK__DOMAIN = "https://auth.${config.modules.services.reverseProxy.hostname}";
            };
            environmentFiles = [
              cfg.envFile
            ];
            pod = pods.authentik-pod.ref;
          };

          authentik-worker.containerConfig = {
            image = "ghcr.io/goauthentik/server:2024.12";
            exec = "worker";
            volumes = [
              "${cfg.dataDir}/media:/media"
              "${cfg.dataDir}/custom-templates:/templates"
              # "${cfg.dataDir}/certs:/certs"
            ];
            environments = {
              AUTHENTIK_REDIS__HOST = "localhost";
              AUTHENTIK_REDIS__PASSWORD = redis_password;
              AUTHENTIK_POSTGRESQL__HOST = "localhost";
              AUTHENTIK_POSTGRESQL__USER = db_user;
              AUTHENTIK_POSTGRESQL__PASSWORD = db_password;
              AUTHENTIK_POSTGRESQL__NAME = db_db;
              AUTHENTIK_DISABLE_STARTUP_ANALYTICS = "true";
              AUTHENTIK_DISABLE_UPDATE_CHECK = "true";
            };
            environmentFiles = [
              cfg.envFile
            ];
            pod = pods.authentik-pod.ref;
          };
        };

        pods.authentik-pod.podConfig = {
          publishPorts = [
            "${toString cfg.publicPort}:9000"
            # "9443:9443"  # HTTPS/TLS port
          ];
        };
      };

    modules.services.reverseProxy.proxies.auth = {
      publicPort = cfg.publicPort;
      auth = false;  # Authentik handles its own auth
    };
  };
}
