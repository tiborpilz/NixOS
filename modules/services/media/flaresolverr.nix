{ config, lib, pkgs, ... }:
with lib;

let
  publicPort = 8191;
in
{
  virtualisation.oci-containers.containers.flaresolverr = {
    image = "ghcr.io/flaresolverr/flaresolverr:latest";
    ports = [ "${toString publicPort}:8191" ];
  };
}
