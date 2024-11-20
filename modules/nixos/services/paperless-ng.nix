{ config, lib, ... }:
with lib;
with lib.my;
let
  publicPort = 8012;
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
      mkdir -p /data/media/paperless/{consume,export}
    '';

    modules.podgroups.pods.paperless-ngx = {
      port = "${toString publicPort}:8000";

      containers.broker = {
        image = "docker.io/redis:6.2";
        volumes = [
          "paperless-redis:/data"
        ];
      };

      containers.db = {
        image = "docker.io/postgres:15";
        volumes = [
          "paperless-pgdata:/var/lib/postgresql/data"
        ];
        environment = {
          "POSTGRES_DB" = db_db;
          "POSTGRES_USER" = db_user;
          "POSTGRES_PASSWORD" = db_password;
        };
      };

      containers.webserver = {
        image = "ghcr.io/paperless-ngx/paperless-ngx:latest";
        dependsOn = [ "db" "broker" ];
        volumes = [
          "/var/lib/paperless/data:/usr/src/paperless/data"
          "media:/usr/src/paperless/media"
          "/data/media/paperless/consume:/usr/src/paperless/consume"
          "/data/media/paperless/export:/usr/src/paperless/export"
        ];
        environment = {
          "PAPERLESS_AUDIT_LOG_ENABLED" = "true";
          "PAPERLESS_REDIS" = "redis://localhost:6379";
          "PAPERLESS_DBHOST" = "localhost";
          "PAPERLESS_CONSUMPTION_DIR" = "/usr/src/paperless/consume";
          "PAPERLESS_OCR_LANGUAGE" = "deu";
          "PAPERLESS_TIKA_ENABLED" = "1";
          "PAPERLESS_TIKA_GOTENBERG_ENDPOINT" = "http://localhost:3000";
          "PAPERLESS_TIKA_ENDPOINT" = "http://localhost:9998";
          "DJANGO_SUPERUSER_PASSOWRD" = "Password";
          "PAPERLESS_ADMIN_USER" = "tibor";
          "PAPERLESS_ADMIN_PASSWORD" = "changeme";
          "PAPERLESS_URL" = "https://paperless.${config.modules.services.reverseProxy.hostname}";
        };
      };

      containers.gotenberg = {
        image = "docker.io/thecodingmachine/gotenberg:8.11.1";
        environment = {
          "DISABLE_GOOGLE_CHROME" = "1";
        };
      };

      containers.tika = {
        image = "ghcr.io/paperless-ngx/tika:latest";
      };
    };
    modules.services.reverseProxy.proxies.paperless = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
