{ config, lib, pkgs, ... }:
with lib;

let
  delugeConfigDir = "/var/lib/deluge/config";
in
  {
    system.activationScripts.makeDelugeDir = stringAfter [ "var" ] ''
      mkdir -p ${delugeConfigDir}
    '';

    virtualisation.oci-containers.containers.deluge = {
      image = "binhex/arch-delugevpn:latest";
      ports = [
        "8112:8112"
        "8118:8118"
        "58846:58846"
        "59846:58946"
      ];
      volumes = [
        "/data/downloads:/data"
        "${delugeConfigDir}:/config"
      ];
      environment = {
        "VPN_ENABLED" = "yes";
        "VPN_USER" = "p4147401";
        "VPN_PASS" = "D!0n4r4p23";
        "VPN_PROV" = "pia";
        "VPN_CLIENT" = "wireguard";
        "STRICT_PORT_FORWARD" = "yes";
        "ENABLE_PRIVOXY" = "yes";
        "LAN_NETWORK" = "192.168.0.0/16,10.88.0.1/16";
        "NAME_SERVERS" = "1.1.1.1";
        "DELUGE_DAEMON_LOG_LEVEL" = "info";
        "DELUGE_WEB_LOG_LEVEL" = "info";
        "DEBUG" = "false";
      };
      extraOptions = [
        "--sysctl=\"net.ipv4.conf.all.src_valid_mark=1\""
        "--privileged=true"
      ];
    };
  }
