{ config, lib, ... }:
with lib;
with lib.my;
let
  fireflyPort = 8210;
  fintsPort = 8211;
  cfg = config.modules.services.firefly-iii;
in
{
  options.modules.services.firefly-iii = {
    enable = mkBoolOpt false;
    baseDir = mkOption {
      description = ''
        The base directory for all Firefly III data.
      '';
      type = types.str;
      default = "/data/firefly-iii";
    };
    fints = mkOption {
      default = {};
      description = ''
        Paths to sops-encrypted files containing the configuration for the respective bank.
      '';
    };
    configDir = mkOption {
      description = ''
        The directory where the configuration files for the FinTS importer are stored.
      '';
      type = types.str;
      default = "${cfg.baseDir}/configurations";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.init-firefly-iii = stringAfter [ "var" ] ''
      # Create directories, if necessary
      mkdir -p ${cfg.baseDir}/storage/{upload,database}
      mkdir -p ${cfg.configDir}

      # Create empty database, if necessary
      touch ${cfg.baseDir}/storage/database/database.sqlite
    '';

    virtualisation.oci-containers.containers.firefly-iii = {
      image = "docker.io/fireflyiii/core:latest";
      ports = ["${toString fireflyPort}:8080"];
      volumes = [
        "${cfg.baseDir}/storage/upload:/var/www/html/storage/upload"
        "${cfg.baseDir}/storage/database:/var/www/html/storage/database"
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
        DB_CONNECTION = "sqlite";
        APP_URL = "http://localhost:${toString fireflyPort}";
      };
    };

    virtualisation.oci-containers.containers.firefly-fints-importer = {
      image = "docker.io/benkl/firefly-iii-fints-importer:latest";
      ports = ["${toString fintsPort}:8080"];
      volumes = [
        "${cfg.configDir}:/app/configurations"
      ];
    };

    modules.services.reverseProxy.proxies.firefly = {
      publicPort = fireflyPort;
      auth = false;
    };

    modules.services.reverseProxy.proxies.firefly-fints-importer = {
      publicPort = fintsPort;
      auth = true;
    };
  };
}
