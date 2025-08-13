{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.media.pinchflat;
  mylib = import ../../../../../lib { inherit inputs lib pkgs; };
  dataDir = "/data/media/pinchflat";
  downloadsDir = "/data/media/downloads";
  publicPort = 8945;
in
with mylib;
{
  options.modules.services.media.pinchflat = {
    enable = mkBoolOpt false;
    pinchflat-version = mkOption {
      type = types.str;
      default = "latest";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.pinchflat = stringAfter [ "var" ] ''
      mkdir -p ${dataDir}
      mkdir -p ${downloadsDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        containers.pinchflat.containerConfig = {
          image = "ghcr.io/kieraneglin/pinchflat:${cfg.pinchflat-version}";
          volumes = [
            "${dataDir}:/config:rw"
            "${downloadsDir}:/downloads:rw"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "UTC";
            ENABLE_PROMETHEUS = true;
          };
          pod = pods.pinchflat-pod.ref;
        };

        pods.pinchflat-pod.podConfig = {
          publishPorts = [
            "${toString publicPort}:8945"
          ];
        };
      };

    modules.services.reverseProxy.proxies.pinchflat = {
      publicPort = publicPort;
      auth = true;
    };
  };
}
