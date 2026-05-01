{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  publicPort = 8285;
  db_user = "recipes";
  db_password = "recipes";
  db_db = "recipes";
  cfg = config.modules.services.tandoor;
  pg_data = "tandoor-pgdata";

  oidcApp = config.modules.services.authentik.applications.tandoor or null;
  oidcEnabled = oidcApp != null;
  oidcProviderId = "authentik";
  tandoorHost = "tandoor.${config.modules.services.reverseProxy.hostname}";
  authHost = "auth.${config.modules.services.reverseProxy.hostname}";
  oidcServerUrl = "https://${authHost}/application/o/tandoor/.well-known/openid-configuration";

  oidcProvidersJson = builtins.toJSON {
    openid_connect = {
      APPS = [{
        provider_id = oidcProviderId;
        name = "Authentik";
        client_id = "%CLIENT_ID%";
        secret = "%CLIENT_SECRET%";
        settings.server_url = oidcServerUrl;
      }];
    };
  };
in
{
  options.modules.services.tandoor = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      system.activationScripts.makeTandoorDir = stringAfter [ "var" ] ''
        mkdir -p /var/lib/tandoor/{staticfiles,mediafiles}
      '';

      system.activationScripts.backupTandoor = stringAfter [ "var" ] ''
        if ${pkgs.podman}/bin/podman volume exists tandoor-pgdata; then
          mkdir -p /data/backups
          version=$(echo "${config.virtualisation.quadlet.containers.tandoor.containerConfig.image}" | rev | cut -d"/" -f1 | rev)
          backup_suffix="$version-$(date +%Y-%m-%d_%H%M%S)"
          db_backup_name="tandoor-pgdata-$version-$(date +%Y-%m-%d_%H%M%S).tar"
          staticfiles_backup_name="tandoor-staticfiles-$version-$(date +%Y-%m-%d_%H%M%S)"
          mediafiles_backup_name="tandoor-mediafiles-$version-$(date +%Y-%m-%d_%H%M%S)"

          ${pkgs.podman}/bin/podman volume export tandoor-pgdata -o /data/backups/$db_backup_name
          cp -r /var/lib/tandoor/staticfiles /data/backups/$staticfiles_backup_name
          cp -r /var/lib/tandoor/mediafiles /data/backups/$mediafiles_backup_name
        fi
        '';

      virtualisation.quadlet =
        let
          inherit (config.virtualisation.quadlet) network pods;
        in
        {
          containers = {
            tandoor-db.containerConfig = {
              image = "docker.io/postgres:14";
              volumes = [
                "${pg_data}:/var/lib/postgresql/data"
              ];
              environments = {
                POSTGRES_USER = db_user;
                POSTGRES_PASSWORD = db_password;
                POSTGRES_DB = db_db;
              };
              pod = pods.tandoor-pod.ref;
            };

            tandoor.containerConfig = {
              image = "docker.io/vabene1111/recipes:2.4.2";
              volumes = [
                "/var/lib/tandoor/staticfiles:/opt/recipes/staticfiles"
                "/var/lib/tandoor/mediafiles:/opt/recipes/mediafiles"
              ];
              environments = {
                SECRET_KEY = "secretkey";
                DB_ENGINE = "django.db.backends.postgresql";
                POSTGRES_HOST = "localhost";
                POSTGRES_PORT = "5432";
                POSTGRES_USER = db_user;
                POSTGRES_PASSWORD = db_password;
                POSTGRES_DB = db_db;
                TANDOOR_PORT = "8080";
                ALLOWED_HOSTS = "*";
                CSRF_TRUSTED_ORIGINS = "https://${tandoorHost}";
              };
              pod = pods.tandoor-pod.ref;
            };
          };

          pods.tandoor-pod.podConfig = {
            publishPorts = [
              "${toString publicPort}:8080"
            ];
          };
        };

      modules.services.reverseProxy.proxies.tandoor = {
        publicPort = publicPort;
        auth = false;
      };
    }

    (mkIf oidcEnabled {
      sops.templates."tandoor-oidc.env" = {
        content = ''
          SOCIAL_PROVIDERS=allauth.socialaccount.providers.openid_connect
          SOCIAL_DEFAULT_ACCESS=1
          SOCIALACCOUNT_PROVIDERS=${
            builtins.replaceStrings
              [ "%CLIENT_ID%" "%CLIENT_SECRET%" ]
              [
                config.sops.placeholder."authentik_tandoor_client_id"
                config.sops.placeholder."authentik_tandoor_client_secret"
              ]
              oidcProvidersJson
          }
        '';
      };

      virtualisation.quadlet.containers.tandoor.containerConfig.environmentFiles = [
        config.sops.templates."tandoor-oidc.env".path
      ];
    })
  ]);
}
