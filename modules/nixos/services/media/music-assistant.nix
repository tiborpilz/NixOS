{ options, config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  # Music Assistant's web UI / API listens on 8095 by default; the audio stream
  # server (referenced by the upstream module's firewall rules) uses 8097.
  webPort = 8095;
  cfg = config.modules.services.media.music-assistant;
in
{
  options.modules.services.media.music-assistant = {
    enable = mkBoolOpt false;

    providers = mkOption {
      type = types.listOf types.str;
      default = [ "chromecast" "dlna" "sonos" "airplay" "snapcast" "spotify" "spotify_connect" "radiobrowser" "tunein" ];
      description = ''
        Music Assistant providers to install system dependencies for. Playback
        targets (Chromecast/DLNA/Sonos/AirPlay) and streaming sources are set up
        here so they are selectable in the web UI. See
        `nix eval nixpkgs#music-assistant.providerNames` for the full list.
      '';
    };
  };

  config = mkIf cfg.enable {
    services.music-assistant = {
      enable = true;
      # klaus runs with the firewall disabled, but keep this correct so provider
      # ports (AirPlay etc.) are opened on any host that does use the firewall.
      openFirewall = true;
      inherit (cfg) providers;
    };

    modules.services.reverseProxy.proxies.music-assistant = {
      publicPort = webPort;
      auth = false;
    };
  };
}
