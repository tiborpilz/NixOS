{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.karakeep;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  oidcApp = config.modules.services.authentik.applications.karakeep or null;
  oidcEnabled = oidcApp != null;
  authHost = "auth.${config.modules.services.reverseProxy.hostname}";
  oidcWellknownUrl = "https://${authHost}/application/o/karakeep/.well-known/openid-configuration";
in
with mylib;
{
  options.modules.services.karakeep = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8639;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/karakeep";
    };
    envFile = mkOption {
      type = types.str;
      description = "Environment file containing NEXTAUTH_SECRET and MEILISEARCH_MASTER_KEY";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      system.activationScripts.initKarakeep = stringAfter [ "var" ] ''
        mkdir -p ${cfg.dataDir}/data
        mkdir -p ${cfg.dataDir}/meilisearch
      '';

      virtualisation.quadlet =
        let inherit (config.virtualisation.quadlet) pods; in
        {
          containers.karakeep.containerConfig = {
            image = "ghcr.io/karakeep-app/karakeep:0.33.2";
            volumes = [
              "${cfg.dataDir}/data:/data"
            ];
            environments = {
              MEILI_ADDR = "http://127.0.0.1:7700";
              BROWSER_WEB_URL = "http://127.0.0.1:9222";
              DATA_DIR = "/data";
              NEXTAUTH_URL = "https://karakeep.${config.modules.services.reverseProxy.hostname}";
              # Authentik decides who gets an account. DISABLE_SIGNUPS would also
              # block it from provisioning them.
              DISABLE_PASSWORD_AUTH = "true";
            };
            environmentFiles = [
              cfg.envFile
            ];
            pod = pods.karakeep-pod.ref;
          };

          containers.karakeep-chrome.containerConfig = {
            image = "ghcr.io/karakeep-app/karakeep-chrome:release";
            exec = [
              "--disable-gpu"
              "--disable-dev-shm-usage"
              "--hide-scrollbars"
              "--disable-blink-features=AutomationControlled"
              "--window-size=1440,900"
            ];
            pod = pods.karakeep-pod.ref;
          };

          containers.karakeep-meilisearch.containerConfig = {
            image = "getmeili/meilisearch:v1.41.0";
            volumes = [
              "${cfg.dataDir}/meilisearch:/meili_data"
            ];
            environments = {
              MEILI_NO_ANALYTICS = "true";
            };
            environmentFiles = [
              cfg.envFile
            ];
            pod = pods.karakeep-pod.ref;
          };

          pods.karakeep-pod.podConfig = {
            publishPorts = [
              "${toString cfg.publicPort}:3000"
            ];
          };
        };

      modules.services.reverseProxy.proxies.karakeep = {
        publicPort = cfg.publicPort;
        auth = false;
      };
    }

    (lib.mkIf oidcEnabled {
      sops.templates."karakeep-oidc.env" = {
        content = ''
          OAUTH_CLIENT_ID=${config.sops.placeholder."authentik_karakeep_client_id"}
          OAUTH_CLIENT_SECRET=${config.sops.placeholder."authentik_karakeep_client_secret"}
          OAUTH_WELLKNOWN_URL=${oidcWellknownUrl}
          OAUTH_PROVIDER_NAME=Authentik
          OAUTH_ALLOW_DANGEROUS_EMAIL_ACCOUNT_LINKING=true
        '';
      };

      virtualisation.quadlet.containers.karakeep.containerConfig.environmentFiles = [
        config.sops.templates."karakeep-oidc.env".path
      ];

      # Route the auth host back to the host.
      virtualisation.quadlet.pods.karakeep-pod.podConfig.addHosts = [
        "${authHost}:host-gateway"
      ];
    })
  ]);
}
