{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.gaggiuino-local-profiler;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.gaggiuino-local-profiler = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8099;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/gaggiuino-local-profiler";
    };
    image = mkOption {
      type = types.str;
      default = "ghcr.io/mxkissnr/gaggiuino-local-profiler/amd64:latest";
    };
    # URL of the Gaggiuino/GaggiMate controller, e.g. http://192.168.1.81.
    # Optional -- left empty, the machine can be configured in-app under
    # Settings -> Machines after first start.
    machineUrl = mkOption {
      type = types.str;
      default = "";
    };
    # Auto-sync interval in minutes (1-60, default 5).
    syncInterval = mkOption {
      type = types.int;
      default = 5;
    };
  };
  config = lib.mkIf cfg.enable {
    system.activationScripts.initGaggiuinoLocalProfiler = stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}
    '';

    virtualisation.oci-containers.containers.gaggiuino-local-profiler = {
      image = cfg.image;
      ports = [ "${toString cfg.publicPort}:8099" ];
      volumes = [
        # Shot database (SQLite), coffee library and bean images -- must
        # persist across container restarts/updates.
        "${cfg.dataDir}:/data"
      ];
      environment = {
        # Empty string behaves exactly like unset (see upstream's
        # docker-compose.standalone.yml), so an unconfigured machineUrl is
        # fine.
        MACHINE_URL = cfg.machineUrl;
        GLP_SYNC_INTERVAL = toString cfg.syncInterval;
      };
    };

    # Short public subdomain: coffee.tiborpilz.xyz. Gated by Cloudflare
    # Access rather than the instance-wide basic auth, like Home Assistant.
    modules.services.reverseProxy.proxies.coffee = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
