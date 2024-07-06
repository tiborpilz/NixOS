{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.linkding;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.linkding = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 9090;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/linkding";
    };
  };
  config = lib.mkIf cfg.enable {
    system.activationScripts.initLinkDing = stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}/data
    '';

    virtualisation.oci-containers.containers.linkding = {
      image = "sissbruecker/linkding:latest";
      ports = [ "${toString cfg.publicPort}:9090" ];
      volumes = [
        "${cfg.dataDir}/data:/etc/linkding/data"
      ];
      environment = {
        LD_CONTAINER_NAME = "linkding";
        LD_HOST_PORT = "9090";
        LD_HOST_DATA_DIR = "./data";
        LD_SUPERUSER_NAME = "admin";
        LD_SUPERUSER_PASSWORD = "changeme";
        LD_DISABLE_BACKGROUND_TASKS = "False";
        LD_DISABLE_URL_VALIDATION = "False";
        LD_ENABLE_AUTH_PROXY = "False";
      };
    };

    modules.services.reverseProxy.proxies.linkding = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
