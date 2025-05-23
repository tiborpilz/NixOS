{ config, lib, pkgs, ... }:
let
  cfg = config.services.samba;
in
{
  services.samba-wsdd.enable = true;
  networking.firewall.allowedTCPPorts = [ 5357 ];
  networking.firewall.allowedUDPPorts = [ 3702 ];
  services.samba = {
    enable = true;
    openFirewall = true;
    securityType = "user";
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "smbnix";
        "netbios name" = "smbnix";
        "security" = "user";
        "hosts allow" = "0.0.0.0/0 192.168.2. 127.0.0.1 localhost";
        "guest account" = "samba";
        "map to guest" = "bad user";
      };
    };
    shares = {
      "media" = {
        path = "/data/media";
        browseable = "yes";
        "guest ok" = "yes";
        "read only" = "no";
        "create mask" = "0777";
        # "force user" = "smbnix";
        # "force group" = "smbnix";
      };
      "downloads" = {
        path = "/data/downloads";
        browseable = "yes";
        "guest ok" = "yes";
        "read only" = "no";
        "create mask" = "0777";
        # "force user" = "smbnix";
        # "force group" = "smbnix";
      };
    };
  };
  users.users.samba = {
    uid = 1001;
    isSystemUser = true;
    group = "samba";
    password = "password";
  };

  users.groups.samba = { };
}
