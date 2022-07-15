{ config, lib, pkgs, ... }:
with lib;
{
  virtualisation.oci-containers.containers.deluge = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    volumes = [ "home-assistant:/config" ];
    environment = {
    };
    extraOptions = [
      "--network=host"
    ];
  };
}
