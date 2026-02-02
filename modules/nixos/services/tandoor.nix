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
in
{
  options.modules.services.tandoor = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
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
            image = "docker.io/postgres:13";
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
            image = "docker.io/vabene1111/recipes:1.5.35";
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
              ALLOWED_HOSTS = "*";
              CSRF_TRUSTED_ORIGINS =
                "https://tandoor.${config.modules.services.reverseProxy.hostname}";
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
  };
}
