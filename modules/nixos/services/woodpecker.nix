{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.woodpecker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  hostname = config.modules.services.reverseProxy.hostname;
  forgejoUrl = "https://forgejo.${hostname}";
  woodpeckerUrl = "https://ci.${hostname}";

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
        WOODPECKER_GRPC_ADDR = ":3008";
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
        WOODPECKER_SERVER = "localhost:3008";
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

      path = with pkgs; [ curl jq coreutils util-linux ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script =
        let
          forgejoBin = lib.getExe config.services.forgejo.package;
          forgejoCfg = config.services.forgejo;
        in
        ''
          set -euo pipefail

          CRED_FILE="${oauthCredFile}"
          APP_NAME="Woodpecker"
          REDIRECT_URI="${woodpeckerUrl}/authorize"
          USERNAME="tibor"
          PASSWORD="$(tr -d '\n' < ${config.sops.secrets.forgejo-admin-password.path})"
          API="${forgeJoUrl}/api/v1"
          BODY_TMP="$(mktemp)"
          trap 'rm -f "$BODY_TMP"' EXIT

          # Wait for Forgejo to be reachable.
          for _ in $(seq 1 60); do
            if curl -sfo /dev/null "$API/version"; then break; fi
            sleep 2
          done

          if [ -s "$CRED_FILE" ]; then
            echo "OAuth credentials already present at $CRED_FILE; nothing to do."
            exit 0
          fi

          # Mint a short-lived admin PAT via the Forgejo CLI (basic auth on the
          # API is unreliable across Forgejo versions, so we go through a token).
          TOKEN_NAME="woodpecker-oauth-setup-$(date +%s%N)"
          TOKEN_RAW="$(runuser -u ${forgejoCfg.user} -- env \
            GITEA_WORK_DIR=${forgejoCfg.stateDir} \
            GITEA_CUSTOM=${forgejoCfg.customDir} \
            ${forgejoBin} admin user generate-access-token \
              --username "$USERNAME" \
              --token-name "$TOKEN_NAME" \
              --scopes "write:user")"
          # Output looks like: "Access token was successfully created... <token>"
          TOKEN="$(printf '%s' "$TOKEN_RAW" | grep -oE '[a-f0-9]{40}' | tail -n1)"
          if [ -z "$TOKEN" ]; then
            echo "Failed to mint Forgejo access token. CLI output:" >&2
            echo "$TOKEN_RAW" >&2
            exit 1
          fi
          AUTH=(-H "Authorization: token $TOKEN")

          # api METHOD PATH [curl-args...] — writes body to $BODY_TMP, errors on non-2xx.
          api() {
            local method="$1"; shift
            local path="$1"; shift
            local code
            code="$(curl -s -o "$BODY_TMP" -w '%{http_code}' \
              "''${AUTH[@]}" -H "Content-Type: application/json" \
              -X "$method" "$@" "$API$path")"
            if [ "$code" -lt 200 ] || [ "$code" -ge 300 ]; then
              echo "Forgejo API $method $path failed (HTTP $code):" >&2
              cat "$BODY_TMP" >&2
              echo >&2
              return 1
            fi
          }

          cleanup_token() {
            # Forgejo rejects token-based auth on token mgmt endpoints
            # ("auth method not allowed"); use basic auth here instead.
            local id code
            code="$(curl -s -o "$BODY_TMP" -w '%{http_code}' \
              -u "$USERNAME:$PASSWORD" "$API/users/$USERNAME/tokens")" || return 0
            [ "$code" -ge 200 ] && [ "$code" -lt 300 ] || return 0
            id="$(jq -r --arg n "$TOKEN_NAME" '.[] | select(.name == $n) | .id' < "$BODY_TMP")"
            [ -n "$id" ] || return 0
            curl -sf -u "$USERNAME:$PASSWORD" -X DELETE \
              "$API/users/$USERNAME/tokens/$id" >/dev/null || true
          }
          # Best-effort cleanup of the temp PAT no matter how we exit.
          trap 'cleanup_token; rm -f "$BODY_TMP"' EXIT

          # Reconcile: drop any prior apps with the same name (we can't recover their secret).
          api GET /user/applications/oauth2
          STALE_IDS="$(jq -r --arg n "$APP_NAME" '.[] | select(.name == $n) | .id' < "$BODY_TMP")"
          for id in $STALE_IDS; do
            echo "Removing stale Forgejo OAuth app id=$id"
            api DELETE "/user/applications/oauth2/$id"
          done

          POST_BODY="$(jq -n --arg name "$APP_NAME" --arg uri "$REDIRECT_URI" \
            '{name: $name, redirect_uris: [$uri], confidential_client: true}')"
          api POST /user/applications/oauth2 -d "$POST_BODY"

          CLIENT_ID="$(jq -r '.client_id // empty' < "$BODY_TMP")"
          CLIENT_SECRET="$(jq -r '.client_secret // empty' < "$BODY_TMP")"
          if [ -z "$CLIENT_ID" ] || [ -z "$CLIENT_SECRET" ]; then
            echo "Forgejo accepted the request but client id/secret missing in response:" >&2
            cat "$BODY_TMP" >&2
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

    modules.services.reverseProxy.proxies.ci = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
