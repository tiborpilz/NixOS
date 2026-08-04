{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  dataDir = "/var/lib/soularr";
  # Soularr insists on reading config.ini from /data, so unlike Lidarr and slskd
  # it cannot mount the downloads at their host path -- /data/downloads/... would
  # nest inside the config mount. Its config file distinguishes the two views
  # anyway: [Slskd] download_dir is Soularr's, [Lidarr] download_dir is Lidarr's.
  completeDir = "/data/downloads/slskd/complete";
  containerCompleteDir = "/downloads";
  publicPort = 8265;

  cfg = config.modules.services.media.soularr;

  # Soularr's config.ini is read by Python's configparser, which wants
  # Python-style booleans rather than nix's `true`/`false`.
  toIni = generators.toINI {
    mkKeyValue = generators.mkKeyValueDefault
      {
        mkValueString =
          v:
          if isBool v then
            (if v then "True" else "False")
          else if isList v then
            concatStringsSep "," (map toString v)
          else
            generators.mkValueStringDefault { } v;
      } " = ";
  };

  # API keys are injected at runtime instead of being interpolated here: the
  # rendered file would otherwise land in the world-readable nix store.
  configTemplate = pkgs.writeText "soularr-config.ini.in" (toIni cfg.settings);

  renderConfig = pkgs.writeShellScript "soularr-render-config" ''
    set -euo pipefail
    umask 077

    lidarrKey="$(tr -d '[:space:]' < ${cfg.lidarrApiKeyFile})"
    slskdKey="$(tr -d '[:space:]' < ${cfg.slskdApiKeyFile})"

    ${pkgs.gnused}/bin/sed \
      -e "s|@LIDARR_API_KEY@|$lidarrKey|" \
      -e "s|@SLSKD_API_KEY@|$slskdKey|" \
      ${configTemplate} > ${dataDir}/config.ini
  '';
in
{
  options.modules.services.media.soularr = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "docker.io/mrusse08/soularr:v1.2.2";

    scriptInterval = mkOption {
      type = types.int;
      default = 300;
      description = "Seconds between Soularr runs.";
    };

    lidarrApiKeyFile = mkOption {
      type = types.str;
      description = ''
        Path to a file containing only Lidarr's API key
        (Lidarr > Settings > General > Security).
      '';
    };

    slskdApiKeyFile = mkOption {
      type = types.str;
      description = ''
        Path to a file containing only slskd's API key. Must be the same value
        as SLSKD_API_KEY in `modules.services.media.slskd.envFile`, and that key
        needs the administrator role.
      '';
    };

    settings = mkOption {
      type = types.attrsOf (types.attrsOf (types.oneOf [ types.str types.int types.bool (types.listOf types.str) ]));
      description = ''
        Contents of Soularr's config.ini. The API key fields are placeholders
        substituted at service start; do not put real keys here.
      '';
      default = { };
    };
  };

  config = mkIf cfg.enable {
    modules.services.media.soularr.settings = {
      Lidarr = mkDefault {
        api_key = "@LIDARR_API_KEY@";
        host_url = "http://localhost:8686";
        # The path as *Lidarr* sees it -- lidarr.nix mounts it at its host path.
        download_dir = completeDir;
        disable_sync = false;
      };

      Slskd = mkDefault {
        api_key = "@SLSKD_API_KEY@";
        host_url = "http://localhost:5030";
        url_base = "/";
        # The path as *Soularr* sees it.
        download_dir = containerCompleteDir;
        delete_searches = false;
        stalled_timeout = 3600;
        remote_queue_timeout = 300;
      };

      "Release Settings" = mkDefault {
        use_selected_lidarr_release = false;
        use_most_common_tracknum = true;
        allow_multi_disc = true;
        accepted_countries = "Europe,Japan,United Kingdom,United States,[Worldwide],Australia,Canada";
        skip_region_check = false;
        accepted_formats = "CD,Digital Media,Vinyl";
      };

      "Search Settings" = mkDefault {
        search_timeout = 5000;
        maximum_peer_queue = 50;
        minimum_peer_upload_speed = 0;
        minimum_filename_match_ratio = "0.8";
        minimum_search_interval = 5;
        # Ordered best-first; Soularr takes the first match it can get.
        allowed_filetypes = "flac 24/192,flac 16/44.1,flac,mp3 320,mp3";
        album_prepend_artist = false;
        search_type = "incrementing_page";
        number_of_albums_to_grab = 30;
        search_source = "missing";
        failed_import_denylist = true;
      };

      "Download Settings" = mkDefault {
        download_filtering = true;
        use_extension_whitelist = false;
        extensions_whitelist = "lrc,nfo,txt";
        rename_download_folders = true;
      };

      Logging = mkDefault {
        level = "INFO";
        log_to_file = false;
      };
    };

    system.activationScripts.soularr = stringAfter [ "var" ] ''
      mkdir -p ${dataDir}
    '';

    systemd.services.soularr-config = {
      description = "Render Soularr's config.ini with API keys from sops";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = renderConfig;
      };
    };

    virtualisation.quadlet.containers.soularr = {
      containerConfig = {
        image = cfg.image;
        # Host networking so `localhost` reaches both Lidarr's and slskd's
        # published ports -- slskd lives behind its own VPN pod and is only
        # reachable via the host, so a shared pod is not an option.
        networks = [ "host" ];
        volumes = [
          "${dataDir}:/data:rw"
          "${completeDir}:${containerCompleteDir}:rw"
          "/etc/localtime:/etc/localtime:ro"
        ];
        environments = {
          TZ = "Europe/Berlin";
          PUID = "0";
          PGID = "0";
          SCRIPT_INTERVAL = toString cfg.scriptInterval;
          WEBUI_ENABLED = "true";
          WEBUI_PORT = toString publicPort;
        };
      };
      unitConfig = {
        Requires = [ "soularr-config.service" ];
        After = [ "soularr-config.service" "lidarr.service" "slskd.service" ];
      };
    };

    modules.services.reverseProxy.proxies.soularr = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
