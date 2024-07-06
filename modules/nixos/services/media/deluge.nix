{ config, lib, pkgs, ... }:
with lib;
with lib.my;

let
  delugeConfigDir = "/var/lib/deluge/config";
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
    sopsFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path to a sops-encrypted file containing env variables for the VPN connection.
      '';
    };
  };

  config = mkIf cfg.enable {
    # system.activationScripts.generateSecretEnv = stringAfter [ "setupSecrets" ] ''
    #   echo VPN_USER=$(cat ${config.sops.secrets.deluge_vpn_user.path}) > ${envFile}
    #   echo VPN_PASS=$(cat ${config.sops.secrets.deluge_vpn_pass.path}) >> ${envFile}
    #   echo PASSWORD=$(cat ${config.sops.secrets.deluge_password.path}) >> ${envFile}
    #   # ${config.sops.secrets.deluge_vpn_user.path}
    #   # ${config.sops.secrets.deluge_vpn_pass.path}
    #   # ${config.sops.secrets.deluge_password.path}
    # '';

    virtualisation.oci-containers.containers.vpn = {
      image = "thrnz/docker-wireguard-pia";
      ports = [
        "${toString publicPort}:8112"
        "8118:8118"
        "6881:6881"
        "58846:58846"
        "53559:53559"
      ];
      environment = {
        "LOC" = "swiss";
        "PORT_FORWARDING" = "1";
        "PORT_PERSIST" = "1";
      };
      extraOptions = [
        "--privileged=true"
        "--cap-add=net_admin"
      ];
    };

    virtualisation.oci-containers.containers.deluge = {
      image = "linuxserver/deluge";
      volumes = [
        "/data/downloads:/downloads"
        "${delugeConfigDir}:/config"
        "/etc/localtime:/etc/localtime:ro"
      ];
      environment = {
        "TZ" = "Europe/Berlin";
        "PUID" = "1000";
        "PGID" = "1000";
      };
      extraOptions = [
        "--network=container:vpn"
      ];
    };

    systemd.services.podman-vpn.serviceConfig.wantedBy = ["podman-deluge.service"];
    systemd.services.podman-vpn.serviceConfig.requiredBy = ["podman-deluge.service"];
    modules.services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
