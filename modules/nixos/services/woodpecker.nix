{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.woodpecker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  hostname = config.modules.services.reverseProxy.hostname;
  forgejoUrl = "https://forgejo.${hostname}";
  woodpeckerUrl = "https://woodpecker.${hostname}";

  oauthStateDir = "/var/lib/woodpecker-forgejo-oauth";
  oauthCredFile = "${oauthStateDir}/credentials.env";
in
with mylib;
{
  options.modules.services.woodpecker = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 3007;
      description = "Port to expose Woodpecker web UI on.";
    };
    envFile = mkOption {
      type = types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    services.woodpecker-server = {
      enable = true;
      environment = {
        WOODPECKER_HOST = woodpeckerUrl;
        WOODPECKER_SERVER_ADDR = ":${toString cfg.publicPort}";
        WOODPECKER_OPEN = "true";
        WOODPECKER_FORGEJO = "true";
        WOODPECKER_FORGEJO_URL = forgejoUrl;
      };
      environmentFile = [ cfg.envFile oauthCredFile ];
    };

    services.woodpecker-agents.agents."docker" = {
      enable = true;
      extraGroups = [ "podman" ];
      environment = {
        WOODPECKER_SERVER = "localhost:9000";
        WOODPECKER_MAX_WORKFLOWS = "4";
        DOCKER_HOST = "unix:///run/podman/podman.sock";
        WOODPECKER_BACKEND = "docker";
      };
      environmentFile = [ cfg.envFile ];
    };

    systemd.services.woodpecker-server = {
      after = [ "woodpecker-forgejo-oauth.service" ];
      requires = [ "woodpecker-forgejo-oauth.service" ];
    };

    systemd.services.woodpecker-forgejo-oauth = {
      description = "Register Woodpecker as an OAuth2 application in Forgejo";
      after = [ "forgejo.service" "network-online.target" ];
      requires = [ "forgejo.service" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [ curl jq coreutils ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script = ''
        set -euo pipefail

        CRED_FILE="${oauthCredFile}"
        APP_NAME="Woodpecker"
        REDIRECT_URI="${woodpeckerUrl}/authorize"
        USERNAME="tibor"
        PASSWORD="$(tr -d '\n' < ${config.sops.secrets.forgejo-admin-password.path})"
        API="${forgejoUrl}/api/v1"

        # Wait for Forgejo to be reachable.
        for _ in $(seq 1 60); do
          if curl -sfo /dev/null "$API/version"; then break; fi
          sleep 2
        done

        if [ -s "$CRED_FILE" ]; then
          echo "OAuth credentials already present at $CRED_FILE; nothing to do."
          exit 0
        fi

        # Reconcile: if an app with our name already exists in Forgejo but we
        # have no stored credentials, we can't recover the secret — delete and
        # recreate.
        APPS="$(curl -sf -u "$USERNAME:$PASSWORD" "$API/user/applications/oauth2")"
        STALE_IDS="$(printf '%s' "$APPS" | jq -r --arg n "$APP_NAME" '.[] | select(.name == $n) | .id')"
        for id in $STALE_IDS; do
          echo "Removing stale Forgejo OAuth app id=$id"
          curl -sf -u "$USERNAME:$PASSWORD" -X DELETE "$API/user/applications/oauth2/$id"
        done

        BODY="$(jq -n --arg name "$APP_NAME" --arg uri "$REDIRECT_URI" \
          '{name: $name, redirect_uris: [$uri], confidential_client: true}')"

        RESPONSE="$(curl -sf -u "$USERNAME:$PASSWORD" -X POST \
          -H "Content-Type: application/json" \
          -d "$BODY" \
          "$API/user/applications/oauth2")"

        CLIENT_ID="$(printf '%s' "$RESPONSE" | jq -r '.client_id // empty')"
        CLIENT_SECRET="$(printf '%s' "$RESPONSE" | jq -r '.client_secret // empty')"

        if [ -z "$CLIENT_ID" ] || [ -z "$CLIENT_SECRET" ]; then
          echo "Failed to register OAuth2 app. API response: $RESPONSE" >&2
          exit 1
        fi

        umask 077
        {
          printf 'WOODPECKER_FORGEJO_CLIENT=%s\n' "$CLIENT_ID"
          printf 'WOODPECKER_FORGEJO_SECRET=%s\n' "$CLIENT_SECRET"
        } > "$CRED_FILE"
        chmod 0440 "$CRED_FILE"
        chown root:woodpecker "$CRED_FILE"

        echo "Registered Woodpecker OAuth2 app in Forgejo (client_id=$CLIENT_ID)"
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${oauthStateDir} 0750 root woodpecker - -"
    ];

    virtualisation.podman = {
      enable = true;
      defaultNetwork.settings = {
        dns_enabled = true;
      };
    };

    networking.firewall.interfaces."podman0" = {
      allowedUDPPorts = [ 53 ];
      allowedTCPPorts = [ 53 ];
    };

    modules.services.reverseProxy.proxies.woodpecker = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
