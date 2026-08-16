{ config, lib, ... }:
with lib;
with lib.my;

let
  appDir = "/var/lib/slskd/app";
  gluetunDir = "/var/lib/gluetun-slskd";
  downloadsDir = "${music.downloadsDir}/slskd";
  publicPort = 5030;
  controlServerPort = 8000;

  music = config.modules.services.media.music;
  cfg = music.slskd;
in
{
  options.modules.services.media.music.slskd = {
    enable = mkBoolOpt false;
    image = mkOpt types.str "docker.io/slskd/slskd:0.26.0";
    gluetunImage = mkOpt types.str "docker.io/qmcgaw/gluetun:v3.41.1";

    serverRegions = mkOption {
      type = types.str;
      default = "Netherlands";
      description = ''
        Comma-separated list of PIA regions gluetun may connect to. Must all
        support port forwarding.
      '';
    };

    credentialsFile = mkOption {
      type = types.str;
      description = ''
        Path to a sops-encrypted env file containing OPENVPN_USER and
        OPENVPN_PASSWORD (PIA account credentials). Must be a separate secret
        from deluge's -- gluetun reads a bare PASSWORD as OPENVPN_PASSWORD.
      '';
    };

    envFile = mkOption {
      type = types.str;
      description = ''
        Path to a sops-encrypted env file. Must contain:

          SLSKD_SLSK_USERNAME, SLSKD_SLSK_PASSWORD
            Soulseek network credentials.

          SLSKD_USERNAME, SLSKD_PASSWORD
            Web UI login. Required: slskd otherwise falls back to slskd/slskd
            on a port published to the LAN.

          SLSKD_API_KEY
            An administrator-role API key.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.slskd = stringAfter [ "var" ] ''
      mkdir -p ${appDir}
      mkdir -p ${gluetunDir}
      mkdir -p ${downloadsDir}/complete
      mkdir -p ${downloadsDir}/incomplete
    '';

    virtualisation.quadlet =
      let
        inherit (config.virtualisation.quadlet) pods;
      in
      {
        # Everything in slskd-pod shares this container's network namespace,
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
              GLUETUN_HTTP_CONTROL_SERVER_ENABLE = "on";
              # Unauthenticated: the control server is only reachable from
              # inside the pod, and slskd is the only other member.
              HTTP_CONTROL_SERVER_AUTH_DEFAULT_ROLE = ''{"auth":"none"}'';
              # The published web UI port has to accept LAN connections.
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
            ];
            environments = {
              TZ = "Europe/Berlin";
              SLSKD_DOWNLOADS_DIR = "${downloadsDir}/complete";
              SLSKD_INCOMPLETE_DIR = "${downloadsDir}/incomplete";

              # PIA's forwarded port is dynamic; slskd polls gluetun's control
              # server for it and will not connect to Soulseek without one.
              SLSKD_VPN = "true";
              SLSKD_VPN_PORT_FORWARDING = "true";
              SLSKD_VPN_GLUETUN_URL = "http://localhost:${toString controlServerPort}";
            };
            environmentFiles = [ cfg.envFile ];
            pod = pods.slskd-pod.ref;
          };
          # The pod's netns exists before gluetun raises tun0, so without this
          # slskd briefly comes up with a working default route.
          unitConfig = {
            Requires = [ "slskd-vpn.service" ];
            After = [ "slskd-vpn.service" ];
          };
        };

        pods.slskd-pod.podConfig = {
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
