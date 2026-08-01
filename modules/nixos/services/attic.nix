{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.attic;
  rp = config.modules.services.reverseProxy;

  # Where the auto-generated JWT signing key lives when no environmentFile is
  # supplied. Kept outside the data dir so wiping the cache can't take every
  # issued token with it.
  generatedKeyFile = "/var/lib/atticd-key/env";

  environmentFile = if cfg.environmentFile != null then cfg.environmentFile else generatedKeyFile;

  # Same settings the service runs with, rendered again so the admin wrapper
  # below can point atticadm at them.
  configFile = (pkgs.formats.toml { }).generate "attic-server.toml" config.services.atticd.settings;
in
{
  options.modules.services.attic = {
    enable = mkBoolOpt false;

    subdomain = mkOption {
      type = types.str;
      default = "cache";
      description = "Subdomain the cache is served on (cache.<hostname>).";
    };

    publicPort = mkOption {
      type = types.int;
      default = 8090;
      description = "Port atticd listens on locally.";
    };

    listenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        Address atticd binds to. Defaults to localhost, so all traffic goes
        through Caddy and the Cloudflare Tunnel.

        Set to "0.0.0.0" if you need to push large store paths: Cloudflare
        caps request bodies at 100 MB, and the attic client uploads a NAR in a
        single request, so pushing anything bigger than that fails through the
        tunnel. Binding to the LAN/tailnet lets you push direct and keep the
        tunnel for pulls.
      '';
    };

    dataDir = mkOption {
      type = types.str;
      default = "/data/attic";
      description = ''
        Directory holding the chunk store and the SQLite database. Defaults to
        the ZFS pool rather than /var/lib, since a binary cache grows without
        much warning.
      '';
    };

    environmentFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        EnvironmentFile providing ATTIC_SERVER_TOKEN_RS256_SECRET_BASE64.

        When null (the default) the key is generated on first start and kept
        at ${generatedKeyFile}. It is not in the repo, so back it up — losing
        it invalidates every token ever issued (the caches themselves survive).
      '';
    };

    garbageCollection = {
      interval = mkOption {
        type = types.str;
        default = "12 hours";
        description = "How often the garbage collector runs.";
      };

      retentionPeriod = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "3 months";
        description = ''
          Retention period for caches that don't set their own. Null keeps
          objects forever, which is what you want until the pool gets
          uncomfortable.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.atticd = {
      enable = true;

      inherit environmentFile;

      settings = {
        listen = "${cfg.listenAddress}:${toString cfg.publicPort}";

        # Clients derive substituter and upload URLs from this, so it has to be
        # the public URL rather than what atticd sees behind Caddy.
        api-endpoint = "https://${cfg.subdomain}.${rp.hostname}/";

        # allowed-hosts is left empty (= allow any Host header) on purpose:
        # requests arrive both as cache.<hostname> via the tunnel and as
        # localhost from this host, and auth is JWT-based either way.

        database.url = "sqlite://${cfg.dataDir}/server.db?mode=rwc";

        storage = {
          type = "local";
          path = "${cfg.dataDir}/storage";
        };

        garbage-collection = {
          interval = cfg.garbageCollection.interval;
        } // optionalAttrs (cfg.garbageCollection.retentionPeriod != null) {
          default-retention-period = cfg.garbageCollection.retentionPeriod;
        };
      };
    };

    # Upstream runs atticd under DynamicUser, whose UID isn't stable across
    # boots — fine for a StateDirectory that systemd re-chowns, wrong for a
    # data dir on the pool. Static user instead.
    users.users.atticd = {
      isSystemUser = true;
      group = "atticd";
      home = cfg.dataDir;
    };

    users.groups.atticd = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0750 atticd atticd -"
      "d ${cfg.dataDir}/storage 0750 atticd atticd -"
    ];

    systemd.services.atticd.serviceConfig = {
      DynamicUser = mkForce false;
      ReadWritePaths = [ cfg.dataDir ];
    };

    # First-boot key generation. The file is written as root and read by
    # systemd before atticd enters its sandbox.
    systemd.services.atticd-key = mkIf (cfg.environmentFile == null) {
      description = "Generate the atticd JWT signing key";
      requiredBy = [ "atticd.service" ];
      before = [ "atticd.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        UMask = "0077";
      };
      script = ''
        if [ ! -s ${generatedKeyFile} ]; then
          mkdir -p "$(dirname ${generatedKeyFile})"
          secret=$(${pkgs.openssl}/bin/openssl genrsa -traditional 4096 | ${pkgs.coreutils}/bin/base64 -w0)
          echo "ATTIC_SERVER_TOKEN_RS256_SECRET_BASE64=$secret" > ${generatedKeyFile}
          chmod 0400 ${generatedKeyFile}
        fi
      '';
    };

    # No basic auth: Nix fetches substituters unauthenticated, and attic does
    # its own token auth on top (public caches stay readable, private ones
    # need a token for pulls too).
    modules.services.reverseProxy.proxies.${cfg.subdomain} = {
      publicPort = cfg.publicPort;
      auth = false;
    };

    environment.systemPackages = [
      pkgs.attic-client

      # Upstream's `atticd-atticadm` hardcodes DynamicUser=yes and therefore
      # fails against the static user above. Same thing, minus that.
      (pkgs.writeShellScriptBin "attic-admin" ''
        exec systemd-run \
          --quiet --pipe --pty --same-dir --wait --collect \
          --service-type=exec \
          --property=EnvironmentFile=${environmentFile} \
          --property=User=atticd \
          --property=Group=atticd \
          --working-directory / \
          -- \
          ${config.services.atticd.package}/bin/atticadm -f ${configFile} "$@"
      '')
    ];
  };
}
