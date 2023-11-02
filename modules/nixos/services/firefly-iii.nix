{ config, lib, ... }:
with lib;
with lib.my;
let
  publicPort = 8210;
  db_root_password = "root";
  db_name = "firefly";
  db_user = "firefly";
  db_password = "firefly";
  cfg = config.modules.services.firefly-iii;
in
{
  options.modules.services.firefly-iii = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.podgroups.pods.firefly-iii = {
      port = "${toString publicPort}:8080";

      containers.app = {
        image = "docker.io/fireflyiii/core:latest";
        volumes = [
          "firefly-iii-upload:/var/www/html/storage/upload"
        ];
        environment = {
          APP_ENV = "local";
          APP_DEBUG = "false";
          SITE_OWNER = "tibor@pilz.berlin";
          APP_KEY = "373971ff2e63f2ea16e8b6efc3e1ee2f"; # TODO: generate this once and store it in a secret
          DEFAULT_LANGUAGE = "en_US";
          DEFAULT_LOCALE = "de_DE";
          TZ = "Europe/Berlin";
          LOG_CHANNEL = "stack";
          APP_LOG_LEVEL = "notice";
          AUDIT_LOG_LEVEL = "emergency";
          DB_CONNECTION = "mysql";
          DB_HOST = "localhost";
          DB_PORT = "3306";
          DB_DATABASE = db_name;
          DB_USERNAME = "root";
          DB_PASSWORD = db_root_password;
          APP_URL = "http://localhost:${toString publicPort}";
        };
      };

      containers.db = {
        image = "docker.io/mariadb:10.5";
        volumes = [
          "firefly-iii-db:/var/lib/mysql"
        ];
        environment = {
          MYSQL_ROOT_PASSWORD = db_root_password;
          MYSQL_DATABASE = db_name;
          MYSQL_USER = db_user;
          MYSQL_PASSWORD = db_password;
        };
      };
    };

    modules.services.reverseProxy.proxies.firefly = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
