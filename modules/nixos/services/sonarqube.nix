{ config, inputs, pkgs, lib, ... }:

# Self-hosted SonarQube (Community Build edition) for code-quality and
# coverage analysis from Woodpecker pipelines.
#
# Login is via Authentik OIDC, using the sonar-auth-oidc community plugin.
# The plugin JAR is fetched at build time and bind-mounted into the
# extensions/plugins directory (overlaying the persistent volume).
#
# Forgejo PR decoration is NOT a built-in SonarQube feature; see
# modules/nixos/services/gitea-sonarqube-bot.nix which polls SQ's
# qualitygates API and posts results to Forgejo PRs.
#
# One-time bootstrap:
#
#  1. After first `nixos-rebuild switch`, log in to https://sonarqube.tiborpilz.xyz
#     with admin/admin and immediately rotate the password (SQ forces this).
#
#  2. The OIDC plugin is pre-loaded but disabled. Configure it under
#     Administration → Configuration → General → OpenID Connect:
#       - Enabled: true
#       - Issuer URI: https://auth.tiborpilz.xyz/application/o/sonarqube/
#       - Client ID: <from sops, see secrets.yaml>
#       - Client Secret: <same>
#       - Login button text: "Login with Authentik"
#       - Auto-login: true (skip the SQ login form on next visit)
#       - Sync groups: optional; if you want Authentik group membership to
#         drive SQ groups, configure on the Authentik side too.
#     OR pre-set via env in this module's `oidc` block to skip the UI step.
#
#  3. Per-repo Woodpecker pipeline:
#       - name: sonarqube
#         image: sonarsource/sonar-scanner-cli:5
#         environment:
#           SONAR_HOST_URL: https://sonarqube.tiborpilz.xyz
#           SONAR_TOKEN:
#             from_secret: sonar_token
#         commands:
#           - sonar-scanner
#             -Dsonar.projectKey=$CI_REPO_NAME
#             -Dsonar.qualitygate.wait=true
#     Token comes from SonarQube → My Account → Security → Generate Token,
#     stored as a Woodpecker secret.

with lib;
let
  cfg = config.modules.services.sonarqube;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  hostname = config.modules.services.reverseProxy.hostname;
  baseUrl = "https://sonarqube.${hostname}";
  authIssuer = "https://auth.${hostname}/application/o/sonarqube/";

  dbUser = "sonar";
  dbName = "sonar";

  oidcPluginVersion = "3.0.0";
  oidcPlugin = pkgs.fetchurl {
    url = "https://github.com/sonar-auth-oidc/sonar-auth-oidc/releases/download/v${oidcPluginVersion}/sonar-auth-oidc-plugin-${oidcPluginVersion}.jar";
    hash = "sha256-8AtkqpO4OhdqQXdhL1yoJgeQGHnrLIDveHTx0yBv/i4=";
  };
in
with mylib;
{
  options.modules.services.sonarqube = {
    enable = mkBoolOpt false;

    publicPort = mkOption {
      type = types.int;
      default = 9000;
      description = "Host port the SonarQube web UI listens on.";
    };

    image = mkOption {
      type = types.str;
      # `community` tag tracks the latest open-source build (renamed from
      # `sonarqube:lts-community`). Pin a specific version once you've
      # validated plugin compatibility.
      default = "sonarqube:community";
    };

    postgresImage = mkOption {
      type = types.str;
      default = "postgres:17-alpine";
    };

    jvmHeap = mkOption {
      type = types.str;
      default = "1g";
      description = ''
        JVM max heap for SonarQube web/CE/search processes. 1g is the
        minimum for small instances; bump to 2g+ for non-trivial use.
      '';
    };

    autoConfigureOidc = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Pre-set the OIDC plugin's Authentik connection via SonarQube env
        vars at container start, so the first login can use SSO without
        a manual UI configuration step.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Elasticsearch (embedded in SonarQube) requires vm.max_map_count >= 524288.
    # NixOS defaults to 1048576 already, so no override needed; we assert
    # to fail loudly if someone forces a lower value.
    assertions = [{
      assertion = (config.boot.kernel.sysctl."vm.max_map_count" or 1048576) >= 524288;
      message = "SonarQube needs vm.max_map_count >= 524288 (Elasticsearch requirement).";
    }];

    sops.secrets = {
      sonarqube_postgres_password = { };
    };

    sops.templates."sonarqube-postgres.env" = {
      content = ''
        POSTGRES_PASSWORD=${config.sops.placeholder.sonarqube_postgres_password}
      '';
    };

    sops.templates."sonarqube-server.env" = {
      content = ''
        SONAR_JDBC_URL=jdbc:postgresql://localhost:5432/${dbName}
        SONAR_JDBC_USERNAME=${dbUser}
        SONAR_JDBC_PASSWORD=${config.sops.placeholder.sonarqube_postgres_password}
        SONAR_WEB_JAVAOPTS=-Xmx${cfg.jvmHeap}
        SONAR_CE_JAVAOPTS=-Xmx${cfg.jvmHeap}
        SONAR_SEARCH_JAVAOPTS=-Xmx${cfg.jvmHeap}
      '' + lib.optionalString cfg.autoConfigureOidc ''
        SONAR_CORE_SERVERBASEURL=${baseUrl}
        SONAR_AUTH_OIDC_ENABLED=true
        SONAR_AUTH_OIDC_ISSUERURI=${authIssuer}
        SONAR_AUTH_OIDC_CLIENTID_SECURED=${config.sops.placeholder."authentik_sonarqube_client_id"}
        SONAR_AUTH_OIDC_CLIENTSECRET_SECURED=${config.sops.placeholder."authentik_sonarqube_client_secret"}
        SONAR_AUTH_OIDC_LOGINBUTTONTEXT=Login with Authentik
        SONAR_AUTH_OIDC_AUTOLOGIN=false
      '';
    };

    virtualisation.quadlet =
      let inherit (config.virtualisation.quadlet) pods; in
      {
        pods.sonarqube-pod.podConfig = {
          publishPorts = [ "${toString cfg.publicPort}:9000" ];
        };

        containers = {
          sonarqube-postgres.containerConfig = {
            image = cfg.postgresImage;
            volumes = [ "sonarqube-pgdata:/var/lib/postgresql/data" ];
            environments = {
              POSTGRES_USER = dbUser;
              POSTGRES_DB = dbName;
            };
            environmentFiles = [
              config.sops.templates."sonarqube-postgres.env".path
            ];
            pod = pods.sonarqube-pod.ref;
          };

          sonarqube-server.containerConfig = {
            image = cfg.image;
            volumes = [
              "sonarqube-data:/opt/sonarqube/data"
              "sonarqube-extensions:/opt/sonarqube/extensions"
              "sonarqube-logs:/opt/sonarqube/logs"
              # Bind-mount the OIDC plugin JAR over the persistent extensions
              # volume so the plugin always matches what Nix declared, while
              # SonarQube's own auto-installed plugins survive across rebuilds.
              "${oidcPlugin}:/opt/sonarqube/extensions/plugins/sonar-auth-oidc-plugin-${oidcPluginVersion}.jar:ro"
            ];
            environmentFiles = [
              config.sops.templates."sonarqube-server.env".path
            ];
            pod = pods.sonarqube-pod.ref;
          };
        };
      };

    modules.services.reverseProxy.proxies.sonarqube = {
      publicPort = cfg.publicPort;
      auth = false;  # SonarQube handles its own auth (via OIDC plugin)
    };
  };
}
