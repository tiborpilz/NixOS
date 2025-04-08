{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.linkwarden;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  db_user = "linkwarden";
  db_password = "linkwarden";
  db_db = "linkwarden";
in
with mylib;
{
  options.modules.services.linkwarden = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8634;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/linkwarden";
    };
    envFile = mkOption {
      type = types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    system.activationScripts.initLinkwarden = stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}/data
    '';

    modules.podgroups.pods.linkwarden = {
      port = "${toString cfg.publicPort}:3000";

      containers.db = {
        image = "postgres:16-alpine";
        volumes = [
          "linkwarden-pgdata:/var/lib/postgresql/data"
        ];
        environment = {
          POSTGRES_USER = db_user;
          POSTGRES_PASSWORD = db_password;
          POSTGRES_DB = db_db;
        };
      };

      containers.linkwarden = {
        image = "ghcr.io/linkwarden/linkwarden:v2.10.0";
        volumes = [
          "${cfg.dataDir}/data:/data/data"
        ];
        environment = {
          "DATABASE_URL" = "postgresql://${db_user}:${db_password}@localhost:5432/${db_db}";
          "NEXTAUTH_URL" = "http://localhost:${toString cfg.publicPort}";
          "NEXTAUTH_SECRET" = "secret";
          "NEXT_PUBLIC_DISABLE_REGISTRATION" = "true";
        };
        environmentFiles = [
          cfg.envFile
        ];
      };
    };

    modules.services.reverseProxy.proxies.linkwarden = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
