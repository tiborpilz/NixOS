{ config, lib, pkgs, ... }:
let
  cfg = config.services.samba;
in
{
  networking.firewall.allowedTCPPorts = [ 5357 ];
  networking.firewall.allowedUDPPorts = [ 3702 ];
  services.samba = {
    enable = true;
    openFirewall = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user
      #use sendfile = yes
      #max protocol = smb2
      hosts allow = 0.0.0.0/0 192.168.2. 127.0.0.1 localhost
      guest account = nobody
    '';
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
    # password = "password";
  };

  users.groups.samba = { };
}
