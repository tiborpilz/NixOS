{ config, lib, ... }:
with lib;
with lib.my;

let
  appDir = "/var/lib/slskd/app";
  gluetunDir = "/var/lib/gluetun-slskd";
  # Same in-container path as in lidarr.nix / soularr.nix -- see the comment there.
  downloadsDir = "/data/downloads/slskd";
  musicDir = "/data/media/music";
  publicPort = 5030;
  controlServerPort = 8000;

  cfg = config.modules.services.media.slskd;
in
{
  options.modules.services.media.slskd = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "docker.io/slskd/slskd:0.26.0";
    gluetunImage = mkOpt types.str "docker.io/qmcgaw/gluetun:v3.41.1";

    serverRegions = mkOption {
      type = types.str;
      default = "Netherlands";
      description = ''
        Comma-separated list of PIA regions gluetun may connect to.
        Must be regions that support port forwarding.
      '';
    };

    credentialsFile = mkOption {
      type = types.str;
      description = ''
        Path to a sops-encrypted env file for this gluetun instance, containing
        OPENVPN_USER and OPENVPN_PASSWORD (PIA account credentials).

        Deliberately a different secret from the one deluge uses: gluetun treats
        a bare PASSWORD as an alias for OPENVPN_PASSWORD.
      '';
    };

    envFile = mkOption {
      type = types.str;
      description = ''
        Path to a sops-encrypted env file for slskd. Must contain:

          SLSKD_SLSK_USERNAME, SLSKD_SLSK_PASSWORD
            Soulseek network credentials.

          SLSKD_USERNAME, SLSKD_PASSWORD
            Web UI login. Not optional in practice: slskd falls back to
            slskd/slskd, and the pod publishes 5030 on the LAN where the
            Caddy basic-auth gate does not apply.

          SLSKD_API_KEY
            An administrator-role API key; Soularr authenticates with it.
      '';
    };

    shareMusicLibrary = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Share ${musicDir} back to the Soulseek network read-only. Soulseek is a
        ratio-aware community; sharing nothing gets you queued behind everyone
        who does.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.slskd = stringAfter [ "var" ] ''
      mkdir -p ${appDir}
      mkdir -p ${gluetunDir}
      mkdir -p ${downloadsDir}/complete
      mkdir -p ${downloadsDir}/incomplete
      mkdir -p ${musicDir}
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        # VPN sidecar. Everything in slskd-pod shares its network namespace,
        # so slskd has no route to the internet that bypasses the tunnel.
        containers.slskd-vpn = {
          containerConfig = {
            image = cfg.gluetunImage;
            volumes = [
              "${gluetunDir}:/gluetun:rw"
            ];
            environments = {
              VPN_SERVICE_PROVIDER = "private internet access";
              VPN_TYPE = "openvpn"; # gluetun's PIA support is openvpn-only
              SERVER_REGIONS = cfg.serverRegions;
              PRIVATE_INTERNET_ACCESS_OPENVPN_ENCRYPTION_PRESET = "normal";
              VPN_PORT_FORWARDING = "on";
              PORT_FORWARD_ONLY = "true";
              VPN_PORT_FORWARDING_STATUS_FILE = "/gluetun/forwarded_port";
              # slskd reads the forwarded port from here rather than from a file,
              # so it can react to a reconnect without a restart.
              GLUETUN_HTTP_CONTROL_SERVER_ENABLE = "on";
              # Unauthenticated on purpose. The control server is bound inside
              # slskd-pod's network namespace, :8000 is never published, and
              # gluetun's firewall does not open it on the tunnel -- so slskd is
              # the only thing that can reach it. An API key here would have to
              # be duplicated into slskd's env file as SLSKD_VPN_GLUETUN_API_KEY
              # and kept in sync, for no reachable attacker.
              HTTP_CONTROL_SERVER_AUTH_DEFAULT_ROLE = ''{"auth":"none"}'';
              # The published web UI port has to accept connections from the LAN.
              # gluetun opens the forwarded port on the tunnel side itself.
              FIREWALL_INPUT_PORTS = toString publicPort;
            };
            environmentFiles = [ cfg.credentialsFile ];
            addCapabilities = [ "NET_ADMIN" ];
            devices = [ "/dev/net/tun" ];
            pod = pods.slskd-pod.ref;
          };
        };

        containers.slskd = {
          containerConfig = {
            image = cfg.image;
            volumes = [
              "${appDir}:/app:rw"
              "${downloadsDir}:${downloadsDir}:rw"
              "/etc/localtime:/etc/localtime:ro"
            ] ++ optional cfg.shareMusicLibrary "${musicDir}:${musicDir}:ro";
            environments = {
              TZ = "Europe/Berlin";
              SLSKD_DOWNLOADS_DIR = "${downloadsDir}/complete";
              SLSKD_INCOMPLETE_DIR = "${downloadsDir}/incomplete";

              # PIA's forwarded port is dynamic. slskd's gluetun integration
              # polls the control server for it and refuses to connect to
              # Soulseek until it has one, which is what we want: no port means
              # no incoming peers and a badly degraded queue position.
              SLSKD_VPN = "true";
              SLSKD_VPN_PORT_FORWARDING = "true";
              SLSKD_VPN_GLUETUN_URL = "http://localhost:${toString controlServerPort}";
            } // optionalAttrs cfg.shareMusicLibrary {
              SLSKD_SHARED_DIR = musicDir;
            };
            environmentFiles = [ cfg.envFile ];
            pod = pods.slskd-pod.ref;
          };
          # The pod's netns exists before gluetun raises tun0, so slskd would
          # otherwise briefly come up on a namespace with a working default route.
          unitConfig = {
            Requires = [ "slskd-vpn.service" ];
            After = [ "slskd-vpn.service" ];
          };
        };

        pods.slskd-pod.podConfig = {
          # Note: gluetun's control server on ${toString controlServerPort} is
          # deliberately not published -- only slskd, inside the pod, talks to it.
          publishPorts = [
            "${toString publicPort}:5030"
          ];
        };
      };

    modules.services.reverseProxy.proxies.slskd = {
      publicPort = publicPort;
      auth = false;
    };
  };
}
