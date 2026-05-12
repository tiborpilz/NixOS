{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.hydra;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  hostname = config.modules.services.reverseProxy.hostname;
in
with mylib;
{
  options.modules.services.hydra = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 3000;
    };
    notificationSender = mkOption {
      type = types.str;
      default = "hydra@${hostname}";
    };
    useSubstitutes = mkBoolOpt true;
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      ensureDatabases = [ "hydra" ];
      ensureUsers = [{
        name = "hydra";
        ensureDBOwnership = true;
      }];
    };

    services.hydra = {
      enable = true;
      hydraURL = "https://hydra.${hostname}";
      listenHost = "localhost";
      port = cfg.publicPort;
      notificationSender = cfg.notificationSender;
      useSubstitutes = cfg.useSubstitutes;
    };

    modules.services.reverseProxy.proxies.hydra = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
