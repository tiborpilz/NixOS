{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.services.excalidraw;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.services.excalidraw = {
    enable = mkBoolOpt false;
    publicPort = mkOption {
      type = types.int;
      default = 8037;
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/excalidraw";
    };
    image = mkOption {
      type = types.str;
      default = "ghcr.io/ozencb/excalidraw-persist:latest";
    };
  };
  config = lib.mkIf cfg.enable {
    system.activationScripts.initExcalidraw = stringAfter [ "var" ] ''
      mkdir -p ${cfg.dataDir}
    '';

    virtualisation.oci-containers.containers.excalidraw = {
      image = cfg.image;
      # nginx serves the client on :80 and proxies /api to the internal
      # server on :4000, so only :80 needs exposing.
      ports = [ "${toString cfg.publicPort}:80" ];
      volumes = [
        "${cfg.dataDir}:/app/data"
      ];
      environment = {
        PORT = "4000";
        NODE_ENV = "production";
        DB_PATH = "/app/data/database.sqlite";
      };
    };

    # The persisted drawings (SQLite in dataDir) are shared by anyone who can
    # reach the instance; access is gated by Cloudflare Access (authentik).
    modules.services.reverseProxy.proxies.excalidraw = {
      publicPort = cfg.publicPort;
      auth = false;
    };
  };
}
