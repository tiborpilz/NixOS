{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.forgejo;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  oidcApp = config.modules.services.authentik.applications.forgejo or null;
  oidcEnabled = oidcApp != null;
  oidcAuthSource = "authentik";
  authHost = "auth.${config.modules.services.reverseProxy.hostname}";
  oidcAutoDiscoverUrl = "https://${authHost}/application/o/forgejo/.well-known/openid-configuration";
in
with mylib;
{
  options.modules.services.forgejo = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 2169;
      description = "Port to expose Forgejo web UI on.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services.forgejo = {
        enable = true;
        database.type = "postgres";
        lfs.enable = true;
        settings = {
          server = {
            DOMAIN = "forgejo.${config.modules.services.reverseProxy.hostname}";
            ROOT_URL = "https://forgejo.${config.modules.services.reverseProxy.hostname}";
            HTTP_PORT = cfg.publicPort;
          };
          service.DISABLE_REGISTRATION = true;
          actions = {
            ENABLED = true;
            DEFAULT_ACTIONS_URL = "github";
          };
        };
      };

      sops.secrets.forgejo-admin-password.owner = "forgejo";
      systemd.services.forgejo.preStart =
        let
          adminCmd = "${lib.getExe config.services.forgejo.package} admin user";
          pwd = config.sops.secrets.forgejo-admin-password;
          user = "tibor";
        in
        ''
          ${adminCmd} create --admin --email "root@localhost" --username ${user} --password "$(tr -d '\n' < ${pwd.path})" || true
          ## uncomment this line to change an admin user which was already created
          ${adminCmd} change-password --username ${user} --password "$(tr -d '\n' < ${pwd.path})" || true
        '';

      environment.systemPackages =
        let
          cfg = config.services.forgejo;
          forgejo-cli = pkgs.writeScriptBin "forgejo-cli" ''
            #!${pkgs.runtimeShell}
            cd ${cfg.stateDir}
            sudo=exec
            if [[ "$USER" != forgejo ]]; then
              sudo='exec /run/wrappers/bin/sudo -u ${cfg.user} -g ${cfg.group} --preserve-env=GITEA_WORK_DIR --preserve-env=GITEA_CUSTOM'
            fi
            # Note that these variable names will change
            export GITEA_WORK_DIR=${cfg.stateDir}
            export GITEA_CUSTOM=${cfg.customDir}
            $sudo ${lib.getExe cfg.package} "$@"
          '';
        in
        [
          forgejo-cli
        ];

      modules.services.reverseProxy.proxies.forgejo = {
        publicPort = cfg.publicPort;
        auth = !oidcEnabled;  # When OIDC is wired up, Forgejo handles its own auth
      };
    }

    (lib.mkIf oidcEnabled {
      sops.secrets.authentik_forgejo_client_id.owner = "forgejo";
      sops.secrets.authentik_forgejo_client_secret.owner = "forgejo";

      services.forgejo.settings = {
        # Allow OIDC users to be auto-provisioned while still blocking the public
        # signup form — only external (OIDC) registration is permitted.
        service.ALLOW_ONLY_EXTERNAL_REGISTRATION = true;
        oauth2_client = {
          ENABLE_AUTO_REGISTRATION = true;
          ACCOUNT_LINKING = "login";
          USERNAME = "nickname";
        };
      };

      systemd.services.forgejo.preStart =
        let
          forgejoBin = lib.getExe config.services.forgejo.package;
          idFile = config.sops.secrets.authentik_forgejo_client_id.path;
          secretFile = config.sops.secrets.authentik_forgejo_client_secret.path;
        in
        lib.mkAfter ''
          existing_id="$(${forgejoBin} admin auth list 2>/dev/null | ${pkgs.gawk}/bin/awk -v n=${oidcAuthSource} 'NR>1 && $2==n {print $1; exit}')"
          oauth_args=(
            --name ${oidcAuthSource}
            --provider openidConnect
            --key "$(tr -d '\n' < ${idFile})"
            --secret "$(tr -d '\n' < ${secretFile})"
            --auto-discover-url ${oidcAutoDiscoverUrl}
          )
          if [ -n "$existing_id" ]; then
            ${forgejoBin} admin auth update-oauth --id "$existing_id" "''${oauth_args[@]}" || true
          else
            ${forgejoBin} admin auth add-oauth "''${oauth_args[@]}" || true
          fi
        '';
    })
  ]);
}
