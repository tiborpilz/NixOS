{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.woodpecker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
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
        WOODPECKER_HOST = "https://wooodpecker.${config.modules.services.reverseProxy.hostname}";
        WOODPECKER_SERVER_ADDR = ":${toString cfg.publicPort}";
        WOODPECKER_OPEN = "true";
      };
      environmentFile = cfg.envFile;
    };

    services.woodpecker-agents.agents."docker" = {
      enable = true;
      extraGroups = [ "podman" ];
      environment = {
        WOODPECKER_SERVER = "localhost:9000";
        WOODPECKER_MAX_WORKFLOWS = "4";
        DOCKER_HOST = "unix:///run/podman/podman.sock";
        WOODPECKER_BACKEND = "docker";
        WOODPECKER_FORGEJO = "true";
        WOODPECKER_FORGEJO_URL = "https://forgejo.${config.modules.services.reverseProxy.hostname}";
      };
      environmentFile = [ cfg.envFile ];
    };

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
      auth = true;
    };
  };
}
