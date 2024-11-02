{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.forgejo;
  publicPort = 3213;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.forgejo = {
    enable = mkBoolOpt false;
    adminPasswordFile = mkOption {
      type = types.path;
    };
  };
  config = lib.mkIf cfg.enable {
    systemd.services.forgejo.preStart = ''
      admin="${lib.getExe config.services.forgejo.package} admin user"
      $admin create --admin --email "root@localhost" --username admin --password "$(tr -d '\n' < ${cfg.adminPasswordFile})" || true
    '';

    services.forgejo = {
      enable = true;
      database.type = "postgres";
      lfs.enable = true;
      settings = {
        server = {
          DOMAIN = "forgejo.${config.modules.services.reverseProxy.hostname}";
          ROOT_URL = "https://forgejo.${config.modules.services.reverseProxy.hostname}";
          HTTP_PORT = 3213;
        };

        service.DISABLE_REGISTRATION = true;

        actions = {
          ENABLED = true;
          DEFAULT_ACTIONS_URL = "github";
        };

        mailer.ENABLED = false;
      };
    };

    modules.services.reverseProxy.proxies.forgejo = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
