{ config, lib, pkgs, ... }:
with lib;
{
  imports = [
    ./sonarr.nix
    ./radarr.nix
    ./plex.nix
    ./deluge.nix
    ./jackett.nix
    ./flaresolverr.nix
  ];

  config = {
    system.activationScripts.makeMediaFolders = stringAfter [ "var" ] ''
      mkdir -p /data/media/{movies,music,tv}
      mkdir -p /data/downloads
    '';
  };
}
