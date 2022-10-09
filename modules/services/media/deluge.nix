{ config, lib, pkgs, ... }:
with lib;

let
  delugeConfigDir = "/var/lib/deluge/config";
  publicPort = 8112;

  cfg = config.services.media.deluge;
in
{
  options.services.media.deluge = {
    enable = mkEnableOption "Deluge";
    sopsFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path to a sops-encrypted file containing env variables for the VPN connection.
      '';
    };

  };

  config = {
    system.activationScripts.makeDelugeDir = stringAfter [ "var" ] ''
      mkdir -p ${delugeConfigDir}
    '';

    system.activationScripts.generateSecretEnv = stringAfter [ "var" ] ''
      echo VPN_USER=$(cat ${config.sops.secrets.deluge_vpn_user.path}) > /tmp/deluge.env
      echo VPN_PASS=$(cat ${config.sops.secrets.deluge_vpn_pass.path}) >> /tmp/deluge.env
      echo PASSWORD=$(cat ${config.sops.secrets.deluge_password.path}) >> /tmp/deluge.env
    '';

    sops.secrets.deluge-env = mkIf (cfg.sopsFile != null) {
      format = "binary";
      sopsFile = cfg.sopsFile;
    };

    sops.secrets.deluge_vpn_user = {};
    sops.secrets.deluge_vpn_pass = {};
    sops.secrets.deluge_password = {};

    virtualisation.oci-containers.containers.deluge = {
      image = "binhex/arch-delugevpn:latest";
      ports = [
        "${toString publicPort}:8112"
        "8118:8118"
        "58846:58846"
        "59846:58946"
      ];
      volumes = [
        "/data/downloads:/downloads"
        "${delugeConfigDir}:/config"
      ];
      environment = {
        "VPN_ENABLED" = "yes";
        "VPN_PROV" = "pia";
        "VPN_CLIENT" = "wireguard";
        "STRICT_PORT_FORWARD" = "yes";
        "ENABLE_PRIVOXY" = "yes";
        "LAN_NETWORK" = "192.168.2.0/24,10.88.0.1/16";
        "NAME_SERVERS" = "1.1.1.1";
        "DELUGE_DAEMON_LOG_LEVEL" = "info";
        "DELUGE_WEB_LOG_LEVEL" = "info";
        "DEBUG" = "false";
      };
      environmentFiles = [
        /tmp/deluge.env
      ];
      extraOptions = [
        "--sysctl=\"net.ipv4.conf.all.src_valid_mark=1\""
        "--privileged=true"
      ];
    };
    services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
