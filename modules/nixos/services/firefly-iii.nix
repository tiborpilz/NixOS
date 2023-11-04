{ config, lib, ... }:
with lib;
with lib.my;
let
  fireflyPort = 8210;
  fintsPort = 8211;
  baseDir = "/data/firefly-iii";
  cfg = config.modules.services.firefly-iii;
in
{
  options.modules.services.firefly-iii = {
    enable = mkBoolOpt false;
    fints = mkOption {
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.init-firefly-iii = stringAfter [ "var" ] ''
      # Create directories, if necessary
      mkdir -p ${baseDir}/storage/{upload,database}
      mkdir -p ${baseDir}/configurations

      # Create configuration files for each bank, so `fints.dkb` will be `configurations/dkb.json` and so on
      for bank in ${lib.attrNames fintsConfigs}; do
        echo "${lib.toPrettyJSON (fintsConfigs.${bank})}" > ${baseDir}/configurations/${bank}.json
      done

      # Create empty database, if necessary
      touch ${baseDir}/storage/database/database.sqlite
    '';

    virtualisation.oci-containers.containers.firefly-iii = {
      image = "docker.io/fireflyiii/core:latest";
      ports = ["${toString fireflyPort}:8080"];
      volumes = [
        "${baseDir}/storage/upload:/var/www/html/storage/upload"
        "${baseDir}/storage/database:/var/www/html/storage/database"
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
        "${baseDir}/configurations:/app/configurations"
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
