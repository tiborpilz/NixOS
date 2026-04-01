{ config, inputs, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.services.frigate;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.frigate = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8971;
      description = "Port to expose Frigate web UI on.";
    };
    configDir = mkOption {
      type = types.str;
      default = "/var/lib/frigate";
    };
    dataDir = mkOption {
      type = types.str;
      default = "/data/frigate";
    };
    enableGPU = mkBoolOpt true;
  };

  config = lib.mkIf cfg.enable {
    system.activationScripts.initFrigate = stringAfter [ "var" ] ''
      mkdir -p ${cfg.configDir}/config
      mkdir -p ${cfg.dataDir}/clips
      mkdir -p ${cfg.dataDir}/recordings
      mkdir -p ${cfg.dataDir}/exports
    '';

    virtualisation.quadlet =
      let inherit (config.virtualisation.quadlet) network pods; in
      {
        containers.frigate.containerConfig = {
          image = "ghcr.io/blakeblackshear/frigate:stable-tensorrt";
          podmanArgs = ["--device=nvidia.com/gpu=all"];
          volumes = [
            "${cfg.configDir}/config:/config"
            "${cfg.dataDir}/clips:/clips"
            "${cfg.dataDir}/recordings:/recordings"
            "${cfg.dataDir}/exports:/exports"
          ];
          environments = {
            FRIGATE_RTSP_PASSWORD = "password";
          };
          pod = pods.frigate-pod.ref;
        };

        pods.frigate-pod.podConfig = {
          publishPorts = [
            "${toString cfg.publicPort}:8971"
          ];
          shmSize = "1G";
        };
      };

    modules.services.reverseProxy.proxies.frigate = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
