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
    system.activationScripts.initDeluge = stringAfter [ "var" ] ''
      # Create config directory
      mkdir -p ${delugeConfigDir}/openvpn

      # Write PIA WG config
#       cat > ${delugeConfigDir}/openvpn/pia.conf << EOF
# client
# dev tun
# proto udp
# remote swiss.privateinternetaccess.com 1198
# resolv-retry infinite
# nobind

      # # Re-Download PIA OpenVPN config files
      # rm ${delugeConfigDir}/openvpn/*
      # mkdir -p ${delugeConfigDir}/openvpn
      # tempdir=$(mktemp -d)
      # ${pkgs.wget}/bin/wget -O $tempdir/openvpn.zip https://www.privateinternetaccess.com/openvpn/openvpn.zip
      # ${pkgs.unzip}/bin/unzip -d $tempdir $tempdir/openvpn.zip
      # mv $tempdir/${cfg.piaCountry}.ovpn ${delugeConfigDir}/openvpn
      # rm -rf $tempdir
    '';

    # system.activationScripts.generateSecretEnv = stringAfter [ "setupSecrets" ] ''
    #   echo VPN_USER=$(cat ${config.sops.secrets.deluge_vpn_user.path}) > ${envFile}
    #   echo VPN_PASS=$(cat ${config.sops.secrets.deluge_vpn_pass.path}) >> ${envFile}
    #   echo PASSWORD=$(cat ${config.sops.secrets.deluge_password.path}) >> ${envFile}
    #   # ${config.sops.secrets.deluge_vpn_user.path}
    #   # ${config.sops.secrets.deluge_vpn_pass.path}
    #   # ${config.sops.secrets.deluge_password.path}
    # '';

    virtualisation.oci-containers.containers.deluge = {
      image = "docker.io/binhex/arch-delugevpn:2.1.1-3-02";
      ports = [
        "${toString publicPort}:8112"
        "8118:8118"
        "58846:58846"
        "59846:58946"
      ];
      volumes = [
        "/data/downloads:/downloads"
        "${delugeConfigDir}:/config"
        "/etc/localtime:/etc/localtime:ro"
      ];
      environment = {
        "VPN_ENABLED" = "yes";
        "VPN_PROV" = "pia";
        "VPN_CLIENT" = "wireguard";
        "STRICT_PORT_FORWARD" = "no";
        "ENABLE_PRIVOXY" = "no";
        "LAN_NETWORK" = "84.200.69.80,37.235.1.174,1.1.1.1,37.235.1.177,84.200.70.40,1.0.0.1";
        "NAME_SERVERS" = "1.1.1.1";
        "DELUGE_DAEMON_LOG_LEVEL" = "info";
        "DELUGE_WEB_LOG_LEVEL" = "info";
        "DEBUG" = "false";
        "PUID" = "0";
        "PGID" = "0";
      };
      environmentFiles = mkIf (cfg.sopsFile != null) [
        (/. + builtins.toPath cfg.sopsFile)
      ];
      extraOptions = [
        "--sysctl=\"net.ipv4.conf.all.src_valid_mark=1\""
        "--privileged=true"
      ];
    };
    modules.services.reverseProxy.proxies.deluge.publicPort = publicPort;
  };
}
