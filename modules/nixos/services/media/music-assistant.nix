{ options, config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  # Music Assistant's web UI / API listens on 8095; the audio stream server uses
  # 8097. Host networking means neither needs publishing.
  webPort = 8095;
  cfg = config.modules.services.media.music-assistant;
in
{
  options.modules.services.media.music-assistant = {
    enable = mkBoolOpt false;

    image = mkOption {
      type = types.str;
      default = "ghcr.io/music-assistant/server:2.9.13";
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/music-assistant";
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.initMusicAssistant = stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}
    '';

    virtualisation.quadlet.containers.music-assistant.containerConfig = {
      image = cfg.image;
      # Players are found over mDNS and the Chromecast/AirPlay providers need
      # LAN multicast, neither of which crosses a bridge network.
      networks = [ "host" ];
      volumes = [
        "${cfg.dataDir}:/data"
      ];
    };

    modules.services.reverseProxy.proxies.music-assistant = {
      publicPort = webPort;
      auth = false;
    };
  };
}
