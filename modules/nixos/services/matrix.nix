{ config, inputs, pkgs, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.matrix;

  hostname = config.modules.services.reverseProxy.hostname;
  matrixHost = "matrix.${hostname}";
  elementHost = "element.${hostname}";
  authHost = "auth.${hostname}";

  # Wire OIDC only when a matching Authentik application is declared (same
  # pattern Forgejo/Tandoor use). Declaring `authentik.applications.matrix`
  # auto-provisions the authentik_matrix_client_{id,secret} sops secrets.
  oidcApp = config.modules.services.authentik.applications.matrix or null;
  oidcEnabled = oidcApp != null;

  # Server-local secrets that never leave the host (macaroon/form/registration).
  # Generated on first boot rather than tracked in sops.
  localSecretsFile = "/var/lib/matrix-synapse/local-secrets.yaml";

  element-web = pkgs.element-web.override {
    conf = {
      default_server_config."m.homeserver" = {
        base_url = "https://${matrixHost}";
        server_name = cfg.serverName;
      };
      brand = "Element";
      disable_guests = true;
    };
  };
in
{
  options.modules.services.matrix = {
    enable = mkBoolOpt false;
    serverName = mkOpt types.str hostname;
    publicPort = mkOpt types.int 8008;
    enableElement = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.matrix-synapse = {
        enable = true;
        settings = {
          server_name = cfg.serverName;
          public_baseurl = "https://${matrixHost}/";
          # Answer federation/client lookups against the delegated host too;
          # the apex (server_name) delegation is served by the Cloudflare
          # Worker in hosts/nixos/klaus/cloudflare/.
          serve_server_wellknown = true;

          report_stats = false;
          enable_registration = false;
          suppress_key_server_warning = true;

          listeners = [{
            port = cfg.publicPort;
            bind_addresses = [ "127.0.0.1" ];
            type = "http";
            tls = false;
            # Caddy already sets X-Forwarded-Proto/For (see reverseProxy.nix).
            x_forwarded = true;
            resources = [{
              names = [ "client" "federation" ];
              compress = false;
            }];
          }];

          database = {
            name = "psycopg2";
            args = {
              user = "matrix-synapse";
              database = "matrix-synapse";
              # Peer auth over the local socket; no password needed.
              host = "/run/postgresql";
            };
          };
        };
        extraConfigFiles = [ localSecretsFile ]
          ++ lib.optional oidcEnabled config.sops.templates."matrix-oidc.yaml".path;
      };

      # Synapse requires its database to use the C collation. The host cluster
      # is already initialised (Forgejo enables Postgres too), so we can't rely
      # on `initialScript` — create the role/DB idempotently once Postgres is up.
      services.postgresql.enable = true;

      systemd.services.matrix-synapse-db-init = {
        description = "Create the Synapse Postgres database (C collation)";
        after = [ "postgresql.service" ];
        requires = [ "postgresql.service" ];
        before = [ "matrix-synapse.service" ];
        requiredBy = [ "matrix-synapse.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "postgres";
        };
        script =
          let psql = "${config.services.postgresql.package}/bin/psql";
          in ''
            ${psql} -tAc "SELECT 1 FROM pg_roles WHERE rolname='matrix-synapse'" | grep -q 1 \
              || ${psql} -c "CREATE ROLE \"matrix-synapse\" WITH LOGIN"
            ${psql} -tAc "SELECT 1 FROM pg_database WHERE datname='matrix-synapse'" | grep -q 1 \
              || ${psql} -c "CREATE DATABASE \"matrix-synapse\" WITH OWNER \"matrix-synapse\" TEMPLATE template0 LC_COLLATE 'C' LC_CTYPE 'C'"
          '';
      };

      # Macaroon/form/registration secrets are host-local; generate them once
      # instead of managing them through sops.
      systemd.services.matrix-synapse-local-secrets = {
        description = "Generate Synapse host-local secrets";
        before = [ "matrix-synapse.service" ];
        requiredBy = [ "matrix-synapse.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          mkdir -p /var/lib/matrix-synapse
          if [ ! -f ${localSecretsFile} ]; then
            {
              echo "registration_shared_secret: \"$(${pkgs.openssl}/bin/openssl rand -hex 32)\""
              echo "macaroon_secret_key: \"$(${pkgs.openssl}/bin/openssl rand -hex 32)\""
              echo "form_secret: \"$(${pkgs.openssl}/bin/openssl rand -hex 32)\""
            } > ${localSecretsFile}
          fi
          chown matrix-synapse:matrix-synapse ${localSecretsFile}
          chmod 600 ${localSecretsFile}
        '';
      };

      modules.services.reverseProxy.proxies.matrix = {
        publicPort = cfg.publicPort;
        # Synapse handles its own auth and serves federation/client API
        # callbacks, so it must bypass basic-auth (and Cloudflare Access).
        auth = false;
      };
    }

    (mkIf cfg.enableElement {
      services.caddy.virtualHosts."${elementHost}" = {
        serverAliases = [ "http://${elementHost}" ];
        extraConfig = ''
          root * ${element-web}
          file_server
        '';
      };
    })

    (mkIf oidcEnabled {
      # Client secret is injected at activation via sops so it never lands in
      # the Nix store. JSON is valid YAML, so Synapse reads this as config.
      sops.templates."matrix-oidc.yaml" = {
        owner = "matrix-synapse";
        content = builtins.toJSON {
          oidc_providers = [{
            idp_id = "authentik";
            idp_name = "Authentik";
            issuer = "https://${authHost}/application/o/matrix/";
            client_id = config.sops.placeholder."authentik_matrix_client_id";
            client_secret = config.sops.placeholder."authentik_matrix_client_secret";
            scopes = [ "openid" "profile" "email" ];
            user_mapping_provider.config = {
              localpart_template = "{{ user.preferred_username }}";
              display_name_template = "{{ user.name }}";
              email_template = "{{ user.email }}";
            };
          }];
        };
      };
    })
  ]);
}
