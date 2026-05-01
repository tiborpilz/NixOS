{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.authentik;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  # Database configuration
  db_user = "authentik";
  db_password = "authentik";
  db_db = "authentik";

  # Redis configuration
  redis_password = "authentik-redis-password";

  envVarPrefix = name: "AUTHENTIK_BP_${lib.toUpper name}";

  mkBlueprint = name: app: ''
    version: 1
    metadata:
      name: nixos-${name}
      labels:
        blueprints.goauthentik.io/instantiate: "true"
    entries:
      - id: provider-${name}
        model: authentik_providers_oauth2.oauth2provider
        identifiers:
          name: ${name}
        attrs:
          client_type: confidential
          client_id: !Env ${envVarPrefix name}_CLIENT_ID
          client_secret: !Env ${envVarPrefix name}_CLIENT_SECRET
          authorization_flow: !Find [authentik_flows.flow, [slug, default-provider-authorization-implicit-consent]]
          invalidation_flow: !Find [authentik_flows.flow, [slug, default-provider-invalidation-flow]]
          signing_key: !Find [authentik_crypto.certificatekeypair, [name, "authentik Self-signed Certificate"]]
          property_mappings:
    ${lib.concatMapStringsSep "\n" (s: "        - !Find [authentik_providers_oauth2.scopemapping, [scope_name, ${s}]]") app.scopes}
          redirect_uris:
    ${lib.concatMapStringsSep "\n" (u: "        - { matching_mode: strict, url: ${builtins.toJSON u} }") app.redirectUris}
      - id: app-${name}
        model: authentik_core.application
        identifiers:
          slug: ${name}
        attrs:
          name: ${app.displayName}
          provider: !KeyOf provider-${name}
  '';

  blueprintsDir = pkgs.runCommand "authentik-blueprints" { } (''
    mkdir -p $out
  '' + lib.concatStringsSep "\n" (lib.mapAttrsToList (name: app:
    "cp ${pkgs.writeText "authentik-blueprint-${name}.yaml" (mkBlueprint name app)} $out/${name}.yaml"
  ) cfg.applications));

  hasApps = cfg.applications != { };
in
with mylib;
{
  options.modules.services.authentik = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 9000;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/authentik";
    };
    envFile = mkOption {
      type = types.str;
      description = "SOPS-encrypted environment file containing AUTHENTIK_SECRET_KEY and other sensitive vars";
    };
    applications = mkOption {
      default = { };
      description = ''
        OIDC applications to declare in Authentik. Each entry creates an OAuth2 provider
        and an application via a blueprint mounted into the Authentik container.

        For each application <name>, two sops secrets are auto-declared:
        - authentik_<name>_client_id
        - authentik_<name>_client_secret

        Add corresponding values to the host's secrets.yaml.
      '';
      type = types.attrsOf (types.submodule ({ name, ... }: {
        options = {
          displayName = mkOption {
            type = types.str;
            default = lib.toUpper (lib.substring 0 1 name) + lib.substring 1 (-1) name;
            description = "Human-readable name shown in the Authentik library";
          };
          redirectUris = mkOption {
            type = types.listOf types.str;
            description = "Allowed OAuth2 redirect URIs (strict match)";
          };
          scopes = mkOption {
            type = types.listOf types.str;
            default = [ "openid" "email" "profile" ];
            description = "Scope mapping names to attach to the provider";
          };
        };
      }));
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      virtualisation.quadlet =
        let inherit (config.virtualisation.quadlet) network pods; in
        {
          containers = {
            authentik-db.containerConfig = {
              image = "postgres:16-alpine";
              volumes = [
                "authentik-pgdata:/var/lib/postgresql/data"
              ];
              environments = {
                POSTGRES_USER = db_user;
                POSTGRES_PASSWORD = db_password;
                POSTGRES_DB = db_db;
              };
              pod = pods.authentik-pod.ref;
            };

            authentik-cache.containerConfig = {
              image = "redis:alpine";
              exec = [
                "--save"
                "60"
                "1"
                "--loglevel"
                "warning"
                "--requirepass"
                redis_password
              ];
              volumes = [
                "authentik-redis:/data"
              ];
              pod = pods.authentik-pod.ref;
            };

            authentik-server.containerConfig = {
              image = "ghcr.io/goauthentik/server:2024.10";
              exec = "server";
              volumes = [
                "${cfg.dataDir}/media:/media"
                "${cfg.dataDir}/custom-templates:/templates"
              ] ++ lib.optional hasApps "${blueprintsDir}:/blueprints/nixos:ro";
              environments = {
                AUTHENTIK_REDIS__HOST = "localhost";
                AUTHENTIK_REDIS__PASSWORD = redis_password;
                AUTHENTIK_POSTGRESQL__HOST = "localhost";
                AUTHENTIK_POSTGRESQL__USER = db_user;
                AUTHENTIK_POSTGRESQL__PASSWORD = db_password;
                AUTHENTIK_POSTGRESQL__NAME = db_db;
                AUTHENTIK_DEFAULT_USER_CHANGE_EMAIL = "false";
                AUTHENTIK_DEFAULT_USER_CHANGE_NAME = "false";
                AUTHENTIK_DEFAULT_USER_CHANGE_USERNAME = "false";
                AUTHENTIK_DISABLE_STARTUP_ANALYTICS = "true";
                AUTHENTIK_DISABLE_UPDATE_CHECK = "true";
                AUTHENTIK_AUTHENTIK__DOMAIN = "https://auth.${config.modules.services.reverseProxy.hostname}";
              };
              environmentFiles = [
                cfg.envFile
              ] ++ lib.optional hasApps config.sops.templates."authentik-blueprints.env".path;
              pod = pods.authentik-pod.ref;
            };

            authentik-worker.containerConfig = {
              image = "ghcr.io/goauthentik/server:2024.10";
              exec = "worker";
              volumes = [
                "${cfg.dataDir}/media:/media"
                "${cfg.dataDir}/custom-templates:/templates"
                # "${cfg.dataDir}/certs:/certs"
              ] ++ lib.optional hasApps "${blueprintsDir}:/blueprints/nixos:ro";
              environments = {
                AUTHENTIK_REDIS__HOST = "localhost";
                AUTHENTIK_REDIS__PASSWORD = redis_password;
                AUTHENTIK_POSTGRESQL__HOST = "localhost";
                AUTHENTIK_POSTGRESQL__USER = db_user;
                AUTHENTIK_POSTGRESQL__PASSWORD = db_password;
                AUTHENTIK_POSTGRESQL__NAME = db_db;
                AUTHENTIK_DISABLE_STARTUP_ANALYTICS = "true";
                AUTHENTIK_DISABLE_UPDATE_CHECK = "true";
              };
              environmentFiles = [
                cfg.envFile
              ] ++ lib.optional hasApps config.sops.templates."authentik-blueprints.env".path;
              pod = pods.authentik-pod.ref;
            };
          };

          pods.authentik-pod.podConfig = {
            publishPorts = [
              "${toString cfg.publicPort}:9000"
              # "9443:9443"  # HTTPS/TLS port
            ];
          };
        };

      modules.services.reverseProxy.proxies.auth = {
        publicPort = cfg.publicPort;
        auth = false;  # Authentik handles its own auth
      };
    }

    (lib.mkIf hasApps {
      sops.secrets = lib.listToAttrs (lib.concatLists (lib.mapAttrsToList (name: _: [
        { name = "authentik_${name}_client_id"; value = { }; }
        { name = "authentik_${name}_client_secret"; value = { }; }
      ]) cfg.applications));

      sops.templates."authentik-blueprints.env" = {
        content = lib.concatStringsSep "\n" (lib.mapAttrsToList (name: _: ''
          ${envVarPrefix name}_CLIENT_ID=${config.sops.placeholder."authentik_${name}_client_id"}
          ${envVarPrefix name}_CLIENT_SECRET=${config.sops.placeholder."authentik_${name}_client_secret"}
        '') cfg.applications);
      };
    })
  ]);
}
