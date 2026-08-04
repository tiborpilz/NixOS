{ config, lib, ... }:
with lib;
with lib.my;

let
  configDir = "/var/lib/prowlarr/config";
  publicPort = 9696;

  cfg = config.modules.services.media.prowlarr;
in
{
  options.modules.services.media.prowlarr = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "lscr.io/linuxserver/prowlarr:2.5.2.5491-ls155";
  };

  config = mkIf cfg.enable {
    system.activationScripts.prowlarr = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        containers.prowlarr.containerConfig = {
          image = cfg.image;
          volumes = [
            "${configDir}:/config:rw"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "Europe/Berlin";
            PUID = "0";
            PGID = "0";
          };
          pod = pods.prowlarr-pod.ref;
        };

        pods.prowlarr-pod.podConfig = {
          publishPorts = [
            "${toString publicPort}:9696"
          ];
        };
      };

    modules.services.reverseProxy.proxies.prowlarr.publicPort = publicPort;
  };
}
