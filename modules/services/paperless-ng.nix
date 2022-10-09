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
    podgroups.pods.paperless-ng = {
      port = "${toString publicPort}:8000";

      containers.db = {
        image = "postgres:13";
        volumes = [ "paperless-pgdata:/var/lib/postgresql/data" ];
        environment = {
          "POSTGRES_DB" = db_db;
          "POSTGRES_USER" = db_user;
          "POSTGRES_PASSWORD" = db_password;
        };
      };

      containers.broker = {
        image = "redis:6.0";
      };

      containers.webserver = {
        image = "jonaswinkler/paperless-ng:latest";
        dependsOn = [ "db" "broker" ];
        volumes = [
          "paperless_data:/usr/src/paperless/data"
          "paperless_media:/usr/src/paperless/media"
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
        image = "thecodingmachine/gotenberg:6.4.4";
        environment = {
          "DISABLE_GOOGLE_CHROME" = "1";
        };
      };

      containers.tika = {
        image = "apache/tika";
      };
    };
    services.reverseProxy.proxies.paperless.publicPort = publicPort;
  };
}
