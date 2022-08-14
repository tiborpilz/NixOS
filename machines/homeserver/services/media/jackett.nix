{ config, lib, pkgs, ... }:
with lib;

let
  jackettConfigDir = "/var/lib/jackett/config";
  publicPort = 9117;
in
{
  system.activationScripts.makejackettDir = stringAfter [ "var" ] ''
    mkdir -p ${jackettConfigDir}
  '';

  virtualisation.oci-containers.containers.jackett = {
    image = "lscr.io/linuxserver/jackett:latest";
    ports = [ "${toString publicPort}:9117" ];
    volumes = [
      "${jackettConfigDir}:/config"
      "/data/media/tv:/tv"
      "/data/downloads:/downloads"
    ];
    environment = {
      "TZ" = "Europe/Berlin";
    };
  };
  reverseProxy.proxies.jackett.publicPort = publicPort;
}
