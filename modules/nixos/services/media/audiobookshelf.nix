{ options, config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  audiobookshelfPort = 8489;
  audiobookshelfConfigDir = "/var/lib/audiobookshelf/config";
  mediaDir = "/data/media/bookshelf";
  cfg = config.modules.services.media.audiobookshelf;
in
{
  options.modules.services.media.audiobookshelf = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeAudiobookshelfDir = stringAfter [ "var" ] ''
      mkdir -p ${audiobookshelfConfigDir}
      mkdir -p ${mediaDir}/audiobooks
      mkdir -p ${mediaDir}/podcasts
      mkdir -p ${mediaDir}/metadata
    '';

    virtualisation.oci-containers.containers.audiobookshelf = {
      image = "ghcr.io/advplyr/audiobookshelf:2.33.1";
      ports = [
        "${toString audiobookshelfPort}:8080"
      ];
      volumes = [
        "${audiobookshelfConfigDir}:/config"
        "${mediaDir}/audiobooks:/books"
        "${mediaDir}/podcasts:/podcasts"
        "${mediaDir}/metadata:/metadata"
      ];
    };
    modules.services.reverseProxy.proxies.audiobookshelf = {
      publicPort = audiobookshelfPort;
      auth = false;
    };
  };
}

