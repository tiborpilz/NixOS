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
      default = "ghcr.io/music-assistant/server:2.9.9";
      description = ''
        Upstream container rather than nixpkgs' services.music-assistant, which
        is stuck on 2.8.7. 2.9.x pins torch, torchaudio, librosa and
        modern_colorthief, so packaging it is not a cheap bump.

        (The original trigger for the bump was a 2.8.7 crash with KeyError
        'refresh_token' when Spotify declined to rotate a refresh token, fixed
        in music-assistant/server#4494. That no longer applies now that the
        Spotify providers are gone, but the packaging argument still holds.)
      '';
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/music-assistant";
      description = ''
        Bind-mounted at /data, which the image's entrypoint already passes as
        --data-dir. Must be a real directory: the old native service used
        DynamicUser, which makes this path a symlink into /var/lib/private.
      '';
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
