{ config, lib, ... }:
with lib;
with lib.my;

let
  publicPort = 8096;
  cfg = config.modules.services.media.jellyfin;
in
{
  options.modules.services.media.jellyfin = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.jellyfin = {
      enable = true;
    };
    modules.services.reverseProxy.proxies.jellyfin = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
