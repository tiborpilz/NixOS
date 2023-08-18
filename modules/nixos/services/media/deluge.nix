{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  delugeConfigDir = "/var/lib/deluge/config";
  envFile = "${delugeConfigDir}/deluge.env";
  publicPort = 8112;

  cfg = config.modules.services.media.deluge;
in
{
  options.modules.services.media.deluge = {
    enable = mkBoolOpt false;
    sopsFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path to a sops-encrypted file containing env variables for the VPN connection.
      '';
    };

  };

  config = mkIf cfg.enable {
    system.activationScripts.makeDelugeDir = stringAfter [ "var" ] ''
      mkdir -p ${delugeConfigDir}
    '';

    system.activationScripts.generateSecretEnv = stringAfter [ "setupSecrets" ] ''
      echo VPN_USER=$(cat ${config.sops.secrets.deluge_vpn_user.path}) > ${envFile}
      echo VPN_PASS=$(cat ${config.sops.secrets.deluge_vpn_pass.path}) >> ${envFile}
      echo PASSWORD=$(cat ${config.sops.secrets.deluge_password.path}) >> ${envFile}
      # ${config.sops.secrets.deluge_vpn_user.path}
      # ${config.sops.secrets.deluge_vpn_pass.path}
      # ${config.sops.secrets.deluge_password.path}
    '';

    sops.secrets.deluge-env = mkIf (cfg.sopsFile != null) {
      format = "binary";
      sopsFile = cfg.sopsFile;
    };

    sops.secrets.deluge_vpn_user = { };
    sops.secrets.deluge_vpn_pass = { };
    sops.secrets.deluge_password = { };

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
        envFile
      ];
      extraOptions = [
        "--sysctl=\"net.ipv4.conf.all.src_valid_mark=1\""
        "--privileged=true"
      ];
    };
    modules.services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
