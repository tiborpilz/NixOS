{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.media.bookorbit;

  db_user = "bookorbit";
  db_password = "bookorbit";
  db_db = "bookorbit";

  appUrl = "https://bookorbit.${config.modules.services.reverseProxy.hostname}";
  authHost = "auth.${config.modules.services.reverseProxy.hostname}";
  oidcEnabled = config.modules.services.authentik.applications ? bookorbit;
in
{
  options.modules.services.media.bookorbit = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8646;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/data/bookorbit";
    };
    delugeDir = mkOption {
      type = types.str;
      default = "/data/downloads/deluge";
    };
    envFile = mkOption {
      type = types.str;
      description = "Environment file containing JWT_SECRET and SETUP_BOOTSTRAP_TOKEN";
    };
    libraries = mkOption {
      type = types.attrsOf types.str;
      default = {
        ebooks = "/data/media/books";
        comics = "/data/media/komga";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      system.activationScripts.initBookorbit = stringAfter [ "var" ] ''
        mkdir -p ${cfg.dataDir}/app ${cfg.dataDir}/postgres ${cfg.dataDir}/inbox ${cfg.dataDir}/downloads
      '';

      virtualisation.quadlet =
        let inherit (config.virtualisation.quadlet) pods; in
        {
          containers.bookorbit-db.containerConfig = {
            image = "docker.io/pgvector/pgvector:pg18";
            volumes = [
              "${cfg.dataDir}/postgres:/var/lib/postgresql/data"
            ];
            environments = {
              POSTGRES_USER = db_user;
              POSTGRES_PASSWORD = db_password;
              POSTGRES_DB = db_db;
              # Subdirectory so the entrypoint can chown PGDATA itself on a
              # root-owned bind mount.
              PGDATA = "/var/lib/postgresql/data/pgdata";
            };
            pod = pods.bookorbit-pod.ref;
          };

          containers.bookorbit.containerConfig = {
            image = "ghcr.io/bookorbit/bookorbit:2.9.0";
            runInit = true;
            volumes = [
              "${cfg.dataDir}/app:/data"
              "${cfg.dataDir}/inbox:/books/inbox"
              "${cfg.dataDir}/downloads:/books/downloads"
              "${cfg.delugeDir}:/downloads/completed"
            ] ++ mapAttrsToList (name: path: "${path}:/books/${name}:ro") cfg.libraries;
            environments = {
              NODE_ENV = "production";
              PORT = "3000";
              POSTGRES_HOST = "localhost";
              POSTGRES_PORT = "5432";
              POSTGRES_USER = db_user;
              POSTGRES_PASSWORD = db_password;
              POSTGRES_DB = db_db;
              APP_URL = appUrl;
              TZ = "Europe/Berlin";
              # The shared media trees are root-owned, as with calibre and readarr.
              PUID = "0";
              PGID = "0";
              LIBRARY_BROWSE_ROOT = "/books";
            };
            environmentFiles = [
              cfg.envFile
            ];
            pod = pods.bookorbit-pod.ref;
          };

          pods.bookorbit-pod.podConfig = {
            publishPorts = [
              "${toString cfg.publicPort}:3000"
            ];
          };
        };

      modules.services.reverseProxy.proxies.bookorbit = {
        publicPort = cfg.publicPort;
        auth = false;
      };
    }

    (mkIf oidcEnabled {
      # Authentik's issuer is a public hostname served by the local Caddy, so
      # the container resolves it back to the host instead of out through
      # Cloudflare. That makes it a private issuer from BookOrbit's view.
      virtualisation.quadlet.pods.bookorbit-pod.podConfig.addHosts = [
        "${authHost}:host-gateway"
      ];

      virtualisation.quadlet.containers.bookorbit.containerConfig.environments.OIDC_ALLOW_LOCAL_ISSUERS = "true";
    })
  ]);
}
