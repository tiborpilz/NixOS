{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  configDir = "/var/lib/komga/config";
  dataDir = "/data/media/komga";
  publicPort = 6732;
  cfg = config.modules.services.media.komga;
in
{
  options.modules.services.media.komga = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeKomgaDirs = stringAfter [ "var" ] ''
      mkdir -p ${configDir} ${dataDir}
    '';

    virtualisation.oci-containers.containers.komga = {
      image = "gotson/komga:latest";
      ports = [ "${toString publicPort}:8080" ];
      volumes = [
        "${configDir}:/config"
        "${dataDir}:/data"
      ];
    };
    modules.services.reverseProxy.proxies.komga = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
