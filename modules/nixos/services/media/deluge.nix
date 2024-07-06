{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  delugeConfigDir = "/var/lib/deluge/config";
  delugeDataDir = "/data/downloads/deluge";
  # envFile = "${delugeConfigDir}/deluge.env";
  publicPort = 8112;

  cfg = config.modules.services.media.deluge;
in
{
  options.modules.services.media.deluge = {
    enable = mkBoolOpt false;
    piaCountry = mkOption {
      type = types.str;
      default = "switzerland";
      description = ''
        The country to connect to.
      '';
    };
    credentialsFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path to a sops-encrypted file containing env variables for the VPN connection.
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
    '';

    virtualisation.oci-containers.containers.deluge = {
      image = "binhex/arch-delugevpn";
      ports = [
        "8112:8112"
        "8118:8118"
        "9118:9118"
        "58846:58846"
        "58946:58946"
        "58946:58946/udp"
      ];
      volumes = [
        "${delugeDataDir}:/data"
        "${delugeConfigDir}:/config"
        "/etc/localtime:/etc/localtime:ro"
      ];
      environment = {
        "VPN_ENABLED" = "yes";
        "VPN_PROV" = "pia";
        "VPN_CLIENT" = "wireguard";
        "ENABLE_STARTUP_SCRIPTS" = "no";
        "ENABLE_PRIVOXY" = "yes";
        "STRICT_PORT_FORWARD" = "yes";
        "USERSPACE_WIREGUARD" = "no";
        "ENABLE_SOCKS" = "yes";
        "SOCKS_USER" = "admin";
        "SOCKS_PASS" = "socks";
        "LAN_NETWORK" = "192.168.2.0/24";
        "NAME_SERVERS" = "84.200.69.80,37.235.1.174,1.1.1.1,37.235.1.177,84.200.70.40,1.0.0.1";
        "DELUGE_DAEMON_LOG_LEVEL" = "info";
        "DELUGE_WEB_LOG_LEVEL" = "info";
        "DELUGE_ENABLE_WEBUI_PASSWORD" = "yes";
        "DEBUG" = "false";
        "UMASK" = "000";
        "PUID" = "0";
        "PGID" = "0";
      };
      environmentFiles = [
        cfg.credentialsFile
      ];
      extraOptions = [
        "--sysctl=net.ipv4.conf.all.src_valid_mark=1"
        "--privileged=true"
      ];
    };

    modules.services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
