{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.forgejo;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
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

  config = lib.mkIf cfg.enable {
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
    in ''
      ${adminCmd} create --admin --email "root@localhost" --username ${user} --password "$(tr -d '\n' < ${pwd.path})" || true
      ## uncomment this line to change an admin user which was already created
      ${adminCmd} change-password --username ${user} --password "$(tr -d '\n' < ${pwd.path})" || true
    '';

    modules.services.reverseProxy.proxies.forgejo = {
      publicPort = cfg.publicPort;
      auth = true;
    };
  };
}

