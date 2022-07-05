{ config, lib, pkgs, ... }:
with lib;
let
  db_user = "paperless";
  db_password = "paperless";
  db_db = "paperless";
in
{
  imports = [
    ../modules/podgroups.nix
  ];

  config = {
    system.activationScripts.makeTandoorDir = stringAfter [ "var" ] ''
      mkdir -p /var/lib/paperless/{staticfiles,mediafiles}
    '';

    podgroups.pods.paperless = {
      port = "8010:8000";

      containers.db = {
        image = "postgres:13";
        volumes = [ "pgdata:/var/lib/postgresql/data" ];
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
  };
}