{ config, pkgs, lib, ... }:
with lib;
{
  imports = [
    ./hardware-configuration.nix
    ./samba.nix
  ];

  config = {
    sops.defaultSopsFile = secrets/secrets.yaml;
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    sops.age.keyFile = "/var/lib/sops-nix/key.txt";
    sops.age.generateKey = true;

    sops.secrets.cloudflared = {
      sopsFile = secrets/secrets.yaml;
      owner = "cloudflared";
    };

    sops.secrets.firefly_import_configs_dkb-private = {
      sopsFile = ./secrets/secrets.yaml;
      path = "${config.modules.services.firefly-iii.configDir}/dkb-private.json";
    };

    sops.secrets.firefly_import_configs_dkb-savings = {
      sopsFile = ./secrets/secrets.yaml;
      path = "${config.modules.services.firefly-iii.configDir}/dkb-savings.json";
    };

    sops.secrets.deluge = {
      sopsFile = ./secrets/secrets.yaml;
    };

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.forceImportRoot = false;
    boot.zfs.extraPools = [ "zpool" ];

    boot.kernelParams = [ "cpufreq.default_governor=powersave" ];

    networking.hostName = "klaus";
    networking.hostId = "a5fdeadb";

    time.timeZone = "Europe/Berlin";


    networking.useDHCP = false;
    networking.wg-quick.interfaces = {
     wg0 = {
       address = [ "10.0.0.2/24" "fdc9:281f:04d7:9ee9::2/64" ];
       # dns = [ "10.0.0.1" "fdc9:281f:04d7:9ee9::1" ];
       privateKeyFile = "/var/lib/wireguard/private.key";

       # Route Plex traffic differently - plex IP is hardcoded and might break
       # preUp = "ip route add 54.246.167.176/32 via 192.168.2.1 dev enp8s0";
       # postDown = "ip route del 54.246.167.176/32 via 192.168.2.1 dev enp8s0";

       peers = [
         {
           publicKey = "QzJm9puVez50UZbCUAJYZnqBdW19o1tBU0Q/WXZsbyw=";

           # TODO: we actually only want to set wireguard for *incoming* connections to klaus
           # allowedIPs = [ "0.0.0.0/0" "::/0" ];
           allowedIPs = [ "10.0.0.0/24" "fdc9:281f:04d7:9ee9::/64" ];
           endpoint = "159.69.194.44:51820";
           persistentKeepalive = 25;
         }
       ];
     };
    };
    networking.firewall.enable = false;
    networking.networkmanager.enable = true;


    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    services.xserver.videoDrivers = ["nvidia"];

    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
      open = false; # The proprietary one is just better :(
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    i18n.defaultLocale = "en_US.UTF-8";

    services.zfs.autoScrub.enable = true;

    services.nfs.server.enable = true;

    fileSystems = {
      "/".label = "nixos-root";
    };

    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "yes";

    programs.zsh.enable = true;

    environment.systemPackages = with pkgs; [
      git
      tmux
      vim
      wireguard-tools
      partition-manager
      gparted
      hdparm
      python3
      htop
    ];

    users.mutableUsers = true;

    users.users.tibor = {
      uid = 1000;
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      shell = pkgs.zsh;
    };

    virtualisation.oci-containers.backend = "podman";
    system.stateVersion = "23.11";

    nix = {
      package = pkgs.nix;
      registry.nixpkgs.flake = pkgs;
    };

    services.avahi = {
      enable = true;
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
        hinfo = true;
        domain = true;
        userServices = true;
      };
      extraServiceFiles = {
        smb = ''
          <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
          <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
          <service-group>
            <name replace-wildcards="yes">%h</name>
            <service>
              <type>_smb._tcp</type>
              <port>445</port>
            </service>
          </service-group>
        '';
      };
    };

    # Seems like a bug in systemd, more info: https://github.com/NixOS/nixpkgs/issues/180175#issuecomment-1273827251
    systemd.services.NetworkManager-wait-online.enable = false;

    home.enable = false;

    modules.services.reverseProxy = {
      enable = true;
      hostname = "tiborpilz.xyz";
      email = "tibor@pilz.berlin";
      tunnelId = "14a104ab-2541-4142-ab22-12908058f156";
      basicAuth = {
        enable = true;
        username = "tibor";
        password = "$2y$05$hchzpHMV8QabeLBTgSjIa.3Nqc7uqblFiQ8WTLKq4xSl4ZmR9rDGu";
      };
    };

    modules.services = {
      syncthing.enable = true;
      tandoor.enable = true;
      paperless.enable = true;
      firefly-iii.enable = true;
      monitoring.enable = false;
      linkding.enable = true;

      nextcloud = {
        enable = true;
        adminpassFile = config.sops.secrets.nextcloud_admin_pass.path;
        dataDir = "/data/nextcloud";
        # home = "/nextcloud";
      };

      media = {
        calibre.enable = true;
        deluge = {
          enable = false;
          sopsFile = config.sops.secrets.deluge.path;
        };
        komga.enable = true;
        immich.enable = true;
        jellyfin.enable = true;
      };
    };

    sops.secrets.nextcloud_admin_pass = mkIf config.modules.services.nextcloud.enable {
      owner = "nextcloud";
    };

    # sops.secrets.storagebox_nextcloud_smb_secrets = {
    #   sopsFile = ./secrets/secrets.yaml;
    #   path = "/etc/nixos/smb-secrets-storagebox-nextcloud";
    # };

    # Usually, filesystems should be part of the hardware configuration. But since
    # this is a remote storage box, it's more bound to the config than the hardware.
    # fileSystems."/nextcloud" = {
    #   device = "//u304118.your-storagebox.de/u304118-sub1";
    #   fsType = "cifs";
    #   options = let
    #     automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    #   in ["${automount_opts},credentials=/etc/nixos/smb-secrets-storagebox-nextcloud,uid=nextcloud,gid=nextcloud"];
    # };

  };
}
