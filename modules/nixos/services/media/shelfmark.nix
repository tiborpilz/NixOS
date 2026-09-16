{ config, lib, ... }:
with lib;
with lib.my;

let
  cfg = config.modules.services.media.shelfmark;
  configDir = "/var/lib/shelfmark/config";
  useExternalBypasser = cfg.flaresolverrUrl != null;
in
{
  options.modules.services.media.shelfmark = {
    enable = mkBoolOpt false;

    publicPort = mkOption {
      type = types.port;
      default = 8085;
      description = "Loopback host port for the Shelfmark web interface";
    };

    image = mkOption {
      type = types.str;
      default = "ghcr.io/calibrain/shelfmark:v1.3.15";
    };

    booksDir = mkOption {
      type = types.str;
      default = "/data/shelfmark/books";
      description = "Book download location";
    };

    mirrors = mkOption {
      type = types.listOf types.str;
      default = [
        "https://annas-archive.gl"
        "https://annas-archive.gd"
        "https://annas-archive.pk"
      ];
      description = "Anna's Archive mirror URLs";
    };

    flaresolverrUrl = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.makeShelfmarkDirs = stringAfter [ "var" ] ''
      mkdir -p ${configDir}
      mkdir -p ${cfg.booksDir}
    '';

    virtualisation.oci-containers.containers.shelfmark = {
      image = cfg.image;
      # Shelfmark's own authentication is disabled. Bind only to loopback so
      # access always passes through the authenticated Caddy reverse proxy.
      ports = [ "127.0.0.1:${toString cfg.publicPort}:8084" ];
      volumes = [
        "${configDir}:/config"
        "${cfg.booksDir}:/books"
        "/data/downloads/deluge:/data"
      ];
      environment = {
        TZ = "Europe/Berlin";
        PUID = "0";
        PGID = "0";
        AUTH_METHOD = "none";
        SEARCH_MODE = "universal";
        INGEST_DIR = "/books";
        DIRECT_DOWNLOAD_ENABLED = "true";
        USE_CF_BYPASS = "true";
        USING_EXTERNAL_BYPASSER = boolToString useExternalBypasser;
        AA_BASE_URL = "auto";
        AA_MIRROR_URLS = concatStringsSep "," cfg.mirrors;
        LIBGEN_MIRROR_URLS = "https://libgen.vg,https://libgen.la,https://libgen.bz,https://libgen.gl,https://libgen.li";
      } // optionalAttrs useExternalBypasser {
        EXT_BYPASSER_URL = cfg.flaresolverrUrl;
        EXT_BYPASSER_PATH = "/v1";
        EXT_BYPASSER_TIMEOUT = "60000";
      };
      extraOptions = [
        # The internal bypasser drives a real Chromium. It crashes on the
        # default 64M of shared memory and wants roughly 2G to itself.
        "--shm-size=2g"
        "--memory=3g"
      ] ++ optionals useExternalBypasser [
        "--add-host=host.containers.internal:host-gateway"
      ];
    };

    modules.services.reverseProxy.proxies.shelfmark.publicPort = cfg.publicPort;
  };
}
