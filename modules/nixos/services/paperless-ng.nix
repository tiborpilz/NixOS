{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  publicPort = 8010;
  db_user = "paperless";
  db_password = "paperless";
  db_db = "paperless";
  cfg = config.modules.services.paperless;
in
{
  options.modules.services.paperless = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makePaperlessDir = stringAfter [ "var" ] ''
      mkdir -p /var/lib/paperless/data
      mkdir -p /data/media/paperless
    '';

    modules.podgroups.pods.paperless-ng = {
      port = "${toString publicPort}:8000";

      containers.db = {
        image = "docker.io/postgres:13";
        volumes = [ "paperless-pgdata:/var/lib/postgresql/data" ];
        environment = {
          "POSTGRES_DB" = db_db;
          "POSTGRES_USER" = db_user;
          "POSTGRES_PASSWORD" = db_password;
        };
      };

      containers.broker = {
        image = "docker.io/redis:6.0";
      };

      containers.webserver = {
        image = "docker.io/jonaswinkler/paperless-ng:latest";
        dependsOn = [ "db" "broker" ];
        volumes = [
          "/var/lib/paperless/data:/usr/src/paperless/data"
          "/data/media/paperless:/usr/src/paperless/media"
        ];
        environment = {
          "PAPERLESS_REDIS" = "redis://localhost:6379";
          "PAPERLESS_DBHOST" = "localhost";
          "PAPERLESS_TIKA_ENABLED" = "1";
          "PAPERLESS_TIKA_GOTENBERG_ENDPOINT" = "http://localhost:3000";
          "PAPERLESS_TIKA_ENDPOINT" = "http://localhost:9998";
          "DJANGO_SUPERUSER_PASSOWRD" = "Password";
          "PAPERLESS_ADMIN_USER" = "tibor";
          "PAPERLESS_ADMIN_PASSWORD" = "changeme";
        };
      };

      containers.gotenberg = {
        image = "docker.io/thecodingmachine/gotenberg:6.4.4";
        environment = {
          "DISABLE_GOOGLE_CHROME" = "1";
        };
      };

      containers.tika = {
        image = "docker.io/apache/tika";
      };
    };
    modules.services.reverseProxy.proxies.paperless = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
