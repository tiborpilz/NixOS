{ config, lib, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.media.shelfarr;
  authHost = "auth.${config.modules.services.reverseProxy.hostname}";
  oidcEnabled = config.modules.services.authentik.applications ? shelfarr;
  oidcIssuer = "https://${authHost}/application/o/shelfarr/";
in
{
  options.modules.services.media.shelfarr = {
    enable = mkBoolOpt false;

    publicPort = mkOption {
      type = types.port;
      default = 5056;
      description = "Loopback host port for the Shelfarr web interface";
    };

    proxyAuth = mkOption {
      type = types.bool;
      default = true;
      description = "Keep Caddy basic authentication in front of Shelfarr during bootstrap";
    };

    image = mkOption {
      type = types.str;
      default = "ghcr.io/pedro-revez-silva/shelfarr:2026.08.31.1";
      description = "Pinned Shelfarr container image";
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/shelfarr";
      description = "Persistent Shelfarr application data directory";
    };

    ebooksDir = mkOption {
      type = types.str;
      default = "/data/shelfarr/ebooks";
      description = "Host-side staging directory for acquired ebooks";
    };

    audiobooksDir = mkOption {
      type = types.str;
      default = "/data/shelfarr/audiobooks";
      description = "Host-side staging directory for acquired audiobooks";
    };

    downloadsDir = mkOption {
      type = types.str;
      default = "/data/downloads/deluge";
      description = "Host-side Deluge data directory";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      system.activationScripts.makeShelfarrDirs = stringAfter [ "var" ] ''
        mkdir -p ${cfg.dataDir}
        mkdir -p ${cfg.ebooksDir}
        mkdir -p ${cfg.audiobooksDir}
      '';

      virtualisation.oci-containers.containers.shelfarr = {
        image = cfg.image;
        ports = [ "127.0.0.1:${toString cfg.publicPort}:80" ];
        volumes = [
          "${cfg.dataDir}:/rails/storage"
          "${cfg.ebooksDir}:/ebooks"
          "${cfg.audiobooksDir}:/audiobooks"
          # Deluge sees this same host directory as /data. Matching container
          # paths lets Shelfarr process completed downloads without a remote
          # path mapping.
          "${cfg.downloadsDir}:/data"
        ];
        environment = {
          PUID = "0";
          PGID = "0";
          CHOWN_ON_START = "never";
          TZ = "Europe/Berlin";
          HTTP_PORT = "80";
          SOLID_QUEUE_IN_PUMA = "1";
        };
        extraOptions = [
          "--add-host=host.containers.internal:host-gateway"
        ] ++ optional oidcEnabled "--add-host=${authHost}:host-gateway";
      };

      modules.services.reverseProxy.proxies.shelfarr = {
        publicPort = cfg.publicPort;
        auth = cfg.proxyAuth;
      };
    }

    (mkIf oidcEnabled {
      sops.templates."shelfarr-oidc.env" = {
        restartUnits = [ "podman-shelfarr.service" ];
        content = ''
          SHELFARR_SETTING_OIDC_ENABLED=true
          SHELFARR_SETTING_OIDC_AUTO_REDIRECT=false
          SHELFARR_SETTING_OIDC_PROVIDER_NAME=Authentik
          SHELFARR_SETTING_OIDC_ISSUER=${oidcIssuer}
          SHELFARR_SETTING_OIDC_CLIENT_ID=${config.sops.placeholder."authentik_shelfarr_client_id"}
          SHELFARR_SETTING_OIDC_CLIENT_SECRET=${config.sops.placeholder."authentik_shelfarr_client_secret"}
          SHELFARR_SETTING_OIDC_SCOPES=openid profile email
          SHELFARR_SETTING_OIDC_LINK_EXISTING_USERS=false
          SHELFARR_SETTING_OIDC_AUTO_CREATE_USERS=true
          SHELFARR_SETTING_OIDC_DEFAULT_ROLE=user
        '';
      };

      virtualisation.oci-containers.containers.shelfarr.environmentFiles = [
        config.sops.templates."shelfarr-oidc.env".path
      ];
    })
  ]);
}
