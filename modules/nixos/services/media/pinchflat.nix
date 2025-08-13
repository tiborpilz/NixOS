{ inputs, pkgs, lib, config, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.media.pinchflat;
  configDir = "/var/lib/pinchflat";
  downloadsDir = "/data/media/pinchflat";
  publicPort = 8945;
in
{
  options.modules.services.media.pinchflat = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.pinchflat = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
      mkdir -p ${downloadsDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        containers.pinchflat.containerConfig = {
          image = "ghcr.io/kieraneglin/pinchflat:v2025.6.6";
          volumes = [
            "${configDir}:/config:rw"
            "${downloadsDir}:/downloads:rw"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "UTC";
            ENABLE_PROMETHEUS = "true";
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
