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
      "backups" = {
        path = "/data/backups/smb";
        browseable = "yes";
        "guest ok" = "no";
        "read only" = "no";
        "valid users" = "smbbackup";
        "force user" = "smbbackup";
        "force group" = "smbbackup";
        "create mask" = "0660";
        "directory mask" = "0770";
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

  users.users.smbbackup = {
    uid = 1010;
    isSystemUser = true;
    group = "smbbackup";
  };

  users.groups.smbbackup = { };

  systemd.tmpfiles.rules = [
    "d /data/backups/smb 0770 smbbackup smbbackup -"
  ];

  sops.secrets.samba_backups_password = {
    sopsFile = ./secrets/secrets.yaml;
    mode = "0400";
  };

  # The samba module has no declarative passdb, so seed it from the secret.
  systemd.services.samba-backups-passdb = {
    description = "Seed samba passdb entry for the backups share";
    wantedBy = [ "multi-user.target" ];
    before = [ "samba-smbd.service" ];
    after = [ "sops-nix.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      pw=$(cat ${config.sops.secrets.samba_backups_password.path})
      printf '%s\n%s\n' "$pw" "$pw" \
        | ${pkgs.samba}/bin/smbpasswd -s -a smbbackup
    '';
  };
}
