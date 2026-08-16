{ config, lib, ... }:
with lib;
with lib.my;

let
  configDir = "/var/lib/lidarr/config";
  musicDir = music.libraryDir;
  downloadsDir = "${music.downloadsDir}/slskd";
  publicPort = 8686;

  music = config.modules.services.media.music;
  cfg = music.lidarr;
in
{
  options.modules.services.media.music.lidarr = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "lscr.io/linuxserver/lidarr:3.1.0.4875-ls37";
  };

  config = mkIf cfg.enable {
    system.activationScripts.lidarr = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
      mkdir -p ${musicDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        containers.lidarr.containerConfig = {
          image = cfg.image;
          volumes = [
            "${configDir}:/config:rw"
            "${musicDir}:${musicDir}:rw"
            "${downloadsDir}:${downloadsDir}:rw"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environments = {
            TZ = "Europe/Berlin";
            PUID = "0";
            PGID = "0";
          };
          pod = pods.lidarr-pod.ref;
        };

        pods.lidarr-pod.podConfig = {
          publishPorts = [
            "${toString publicPort}:8686"
          ];
        };
      };

    modules.services.reverseProxy.proxies.lidarr.publicPort = publicPort;
  };
}
