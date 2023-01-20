{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  publicPort = 8191;
  cfg = config.modules.services.media.flaresolverr;
in
{
  options.modules.services.media.flaresolverr = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    virtualisation.oci-containers.containers.flaresolverr = {
      image = "ghcr.io/flaresolverr/flaresolverr:latest";
      ports = [ "${toString publicPort}:8191" ];
    };
  };
}
