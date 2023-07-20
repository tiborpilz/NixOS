{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  publicPort = 8285;
  db_user = "tandoor";
  db_password = "tandoor";
  db_db = "tandoor";
  cfg = config.modules.services.tandoor;
in
{
  options.modules.services.tandoor = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeTandoorDir = stringAfter [ "var" ] ''
      mkdir -p /var/lib/tandoor/{staticfiles,mediafiles}
    '';

    modules.podgroups.pods.tandoor = {
      port = "${toString publicPort}:8080";

      containers.db = {
        image = "postgres:13";
        volumes = [ "tandoor-pgdata:/var/lib/postgresql/data" ];
        environment = {
          "POSTGRES_DB" = db_db;
          "POSTGRES_USER" = db_user;
          "POSTGRES_PASSWORD" = db_password;
        };
      };

      containers.tandoor = {
        image = "vabene1111/recipes:0.16.8";
        environment = {
          "SECRET_KEY" = "secretkey";
          "DB_ENGINE" = "django.db.backends.postgresql";
          "POSTGRES_HOST" = "localhost";
          "POSTGRES_PORT" = "5432";
          "POSTGRES_USER" = db_user;
          "POSTGRES_PASSWORD" = db_password;
          "POSTGRES_DB" = db_db;
        };
        volumes = [
          "/var/lib/tandoor/staticfiles:/opt/recipes/staticfiles"
          "/var/lib/tandoor/mediafiles:/opt/recipes/mediafiles"
        ];
      };
    };
    modules.services.reverseProxy.proxies.tandoor.publicPort = publicPort;
  };
}