{ config, lib, ... }:
with lib;
with lib.my;

let
  photoprismDataDir = "/data/media/photoprism";
  publicPort = 2342;
  cfg = config.modules.services.media.photoprism;
  db_name = "photoprism";
  db_user = "photoprism";
  db_password = "photoprism";
  db_root_password = "photoprism";
in
{
  options.modules.services.media.photoprism = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makePhotoprismDir = stringAfter [ "var" ] ''
      mkdir -p ${photoprismDataDir}/{originals,import,storage}
    '';

    modules.podgroups.pods.photoprism = {
      port = "${toString publicPort}:2342";

      containers.photoprism = {
        image = "photoprism/photoprism:latest";
        volumes = [
          "${photoprismDataDir}/originals:/photoprism/originals"
          "${photoprismDataDir}/import:/photoprism/import"
          "${photoprismDataDir}/storage:/photoprism/storage"
        ];
        environment = {
          "PHOTOPRISM_ADMIN_USER" = "admin";                 # admin login username
          "PHOTOPRISM_ADMIN_PASSWORD" = "insecure";          # initial admin password (8-72 characters)
          "PHOTOPRISM_AUTH_MODE" = "password";               # authentication mode (public, password)
          "PHOTOPRISM_SITE_URL" = "https://photoprism.${config.modules.services.reverseProxy.hostname}";
          "PHOTOPRISM_DISABLE_TLS" = "true";                # disables HTTPS/TLS even if the site URL starts with https:// and a certificate is available
          "PHOTOPRISM_DEFAULT_TLS" = "false";                 # defaults to a self-signed HTTPS/TLS certificate if no other certificate is available
          "PHOTOPRISM_ORIGINALS_LIMIT" = "5000";               # file size limit for originals in MB (increase for high-res video)
          "PHOTOPRISM_HTTP_COMPRESSION" = "gzip";            # improves transfer speed and bandwidth utilization (none or gzip)
          "PHOTOPRISM_LOG_LEVEL" = "info";                   # log level: trace, debug, info, warning, error, fatal, or panic
          "PHOTOPRISM_READONLY" = "false";                   # do not modify originals directory (reduced functionality)
          "PHOTOPRISM_EXPERIMENTAL" = "false";               # enables experimental features
          "PHOTOPRISM_DISABLE_CHOWN" = "false";              # disables updating storage permissions via chmod and chown on startup
          "PHOTOPRISM_DISABLE_WEBDAV" = "false";             # disables built-in WebDAV server
          "PHOTOPRISM_DISABLE_SETTINGS" = "false";           # disables settings UI and API
          "PHOTOPRISM_DISABLE_TENSORFLOW" = "false";         # disables all features depending on TensorFlow
          "PHOTOPRISM_DISABLE_FACES" = "false";              # disables face detection and recognition (requires TensorFlow)
          "PHOTOPRISM_DISABLE_CLASSIFICATION" = "false";     # disables image classification (requires TensorFlow)
          "PHOTOPRISM_DISABLE_VECTORS" = "false";            # disables vector graphics support
          "PHOTOPRISM_DISABLE_RAW" = "false";                # disables indexing and conversion of RAW images
          "PHOTOPRISM_RAW_PRESETS" = "false";                # enables applying user presets when converting RAW images (reduces performance)
          "PHOTOPRISM_JPEG_QUALITY" = "85";                    # a higher value increases the quality and file size of JPEG images and thumbnails (25-100)
          "PHOTOPRISM_DETECT_NSFW" = "false";                # automatically flags photos as private that MAY be offensive (requires TensorFlow)
          "PHOTOPRISM_UPLOAD_NSFW" = "true";                 # allows uploads that MAY be offensive (no effect without TensorFlow);;;;;;;;
          "PHOTOPRISM_DATABASE_DRIVER" = "mysql";            # use MariaDB 10.5+ or MySQL 8+ instead of SQLite for improved performance
          "PHOTOPRISM_DATABASE_SERVER" = "localhost:3306";   # MariaDB or MySQL database server (hostname:port)
          "PHOTOPRISM_DATABASE_NAME" = db_name;              # MariaDB or MySQL database schema name
          "PHOTOPRISM_DATABASE_USER" = db_user;               # MariaDB or MySQL database user name
          "PHOTOPRISM_DATABASE_PASSWORD" = db_password;       # MariaDB or MySQL database user password
          "PHOTOPRISM_SITE_CAPTION" = "AI-P;owered Photos App";
          "PHOTOPRISM_SITE_DESCRIPTION" = "";                # meta site description
          "PHOTOPRISM_SITE_AUTHOR" = "";                     # meta site author
        };
      };

      containers.db = {
        image = "mariadb:11";
        volumes = [
          "photoprism-db:/var/lib/mysql"
        ];
        # command = [
        #   "--innodb-buffer-pool-size=2G"
        #   "--lower-case-table-names=1"
        #   "--transaction-isolation=READ-COMMITTED"
        #   "--character-set-server=utf8mb4"
        #   "--collation-server=utf8mb4_unicode_ci"
        #   "--max-connections=512"
        #   "--innodb-rollback-on-timeout=OFF"
        #   "--innodb-lock-wait-timeout=180"
        # ];
        environment = {
          "MARIADYB_AUTO_UPGRADE" = "1";
          "MARIADB_INITDB_SKIP_TZINFO" = "1";
          "MARIADB_DATABASE" = db_name;
          "MARIADB_USER" = db_user;
          "MARIADB_PASSWORD" = db_password;
          "MARIADB_ROOT_PASSWORD" = db_root_password;
        };
      };
    };

    modules.services.reverseProxy.proxies.photoprism = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
