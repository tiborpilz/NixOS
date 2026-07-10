{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  delugeConfigDir = "/var/lib/deluge/config";
  delugeDataDir = "/data/downloads/deluge";
  gluetunDataDir = "/var/lib/gluetun";
  publicPort = 8112;

  cfg = config.modules.services.media.deluge;
in
{
  options.modules.services.media.deluge = {
    enable = mkBoolOpt false;
    autoStart = mkBoolOpt true;
    serverRegions = mkOption {
      type = types.str;
      default = "Netherlands";
      description = ''
        Comma-separated list of PIA regions gluetun may connect to.
        Must be regions that support port forwarding.
      '';
    };
    credentialsFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path to a sops-encrypted env file containing OPENVPN_USER and
        OPENVPN_PASSWORD (PIA account credentials) for gluetun.
      '';
    };
    delugePassword = mkOption {
      type = types.str;
      default = "deluge";
      description = ''
        The password for the deluge web interface.
      '';
    };
  };

  config = mkIf cfg.enable {
    system.activationScripts.createDelugeDirs = stringAfter [ "var" ] ''
      mkdir -p ${delugeConfigDir}
      mkdir -p ${delugeDataDir}
      mkdir -p ${gluetunDataDir}
    '';

    # VPN sidecar.
    virtualisation.oci-containers.containers.deluge-vpn = {
      image = "qmcgaw/gluetun:v3.41.1";
      # All ports for the shared network namespace are published here,
      # not on the deluge container.
      ports = [
        "8112:8112" # deluge web UI
        "58846:58846" # deluge daemon RPC (sonarr/radarr)
      ];
      volumes = [
        "${gluetunDataDir}:/gluetun"
      ];
      environment = {
        "VPN_SERVICE_PROVIDER" = "private internet access";
        "VPN_TYPE" = "openvpn"; # gluetun's PIA support is openvpn-only
        "SERVER_REGIONS" = cfg.serverRegions;
        "PRIVATE_INTERNET_ACCESS_OPENVPN_ENCRYPTION_PRESET" = "normal";
        "VPN_PORT_FORWARDING" = "on";
        "PORT_FORWARD_ONLY" = "true";
        # Written to ${gluetunDataDir}/forwarded_port on the host; deluge's
        # listen port has to be pointed at it for incoming peers.
        "VPN_PORT_FORWARDING_STATUS_FILE" = "/gluetun/forwarded_port";
        # Published ports that must accept connections from the LAN
        # (web UI, daemon RPC).
        "FIREWALL_INPUT_PORTS" = "8112,58846";
      };
      environmentFiles = [
        cfg.credentialsFile
      ];
      extraOptions = [
        "--cap-add=NET_ADMIN"
        "--device=/dev/net/tun"
      ];
    };

    virtualisation.oci-containers.containers.deluge = {
      image = "lscr.io/linuxserver/deluge:2.2.0";
      inherit (cfg) autoStart;
      dependsOn = [ "deluge-vpn" ];
      volumes = [
        "${delugeDataDir}:/data"
        "${delugeConfigDir}:/config"
        "/etc/localtime:/etc/localtime:ro"
      ];
      environment = {
        "PUID" = "0";
        "PGID" = "0";
        "UMASK" = "000";
        "DELUGE_LOGLEVEL" = "info";
      };
      extraOptions = [
        "--network=container:deluge-vpn"
      ];
    };

    modules.services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
