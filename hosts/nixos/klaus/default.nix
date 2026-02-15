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

    sops.secrets.nextcloud_admin_pass = mkIf config.modules.services.nextcloud.enable {
      owner = "nextcloud";
    };

    sops.secrets.nixAccessTokens = {
      sopsFile = ./secrets/secrets.yaml;
      mode = "0400";
      group = config.users.groups.keys.name;
    };

    sops.secrets.authentikEnv = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "authentik";
    };

    sops.secrets.linkwardenEnv = {
      sopsFile = ./secrets/secrets.yaml;
    };

    sops.secrets.woodpeckerEnv = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "woodpecker";
    };

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.forceImportRoot = false;
    boot.zfs.extraPools = [ "zpool" ];

    boot.kernelParams = [ "cpufreq.default_governor=conservative" ];

    boot.tmp.useTmpfs = false;


    systemd.services.refresh-flake = {
      description = "Update the remote flake used for system configuration";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.nixVersions.stable}/bin/nix flake prefetch github:tiborpilz/nixos --refresh";
      };
    };

    # systemd.services.nixos-upgrade.wants = [ "refresh-flake.service" ];

    # system.autoUpgrade = {
    #   enable = true;
    #   flake = "github:tiborpilz/nixos";
    #   flags = [
    #     "-L"
    #   ];
    #   dates = "02:00";
    #   "randomizedDelaySec" = "45min";
    # };

    powerManagement.cpuFreqGovernor = "conservative";

    networking.hostName = "klaus";
    networking.hostId = "a5fdeadb";

    time.timeZone = "Europe/Berlin";


    networking.useDHCP = false;
    networking.firewall.enable = false;
    networking.networkmanager.enable = true;


    hardware.opengl = {
      enable = true;
      driSupport32Bit = true;
    };

    services.xserver.videoDrivers = [ "nvidia" ];

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

    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "yes";

    services.wyoming.faster-whisper.servers.ha = {
      enable = true;
      language = "en";
      uri = "tcp://0.0.0.0:10300";
    };

    programs.zsh.enable = true;

    environment.systemPackages = with pkgs; [
      git
      tmux
      vim
      wireguard-tools
      gparted
      hdparm
      python3
      htop
    ];

    # TODO: rotate github nix access token
    # nix.extraOptions = ''
    #   !include ${config.sops.secrets.nixAccessTokens.path}
    # '';

    users.mutableUsers = true;

    users.groups.authentik = {};
    users.users.authentik = {
      uid = 1005;
      isSystemUser = true;
      group = "authentik";
    };

    users.groups.cloudflared = {};
    users.users.cloudflared = {
      uid = 1006;
      isSystemUser = true;
      group = "cloudflared";
      home = "/var/lib/cloudflared";
      shell = pkgs.zsh;
    };

    users.groups.woodpecker = {};
    users.users.woodpecker = {
      uid = 1007;
      isSystemUser = true;
      group = "woodpecker";
      home = "/var/lib/woodpecker";
      shell = pkgs.zsh;
    };

    users.users.tibor = {
      uid = 1000;
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      shell = pkgs.zsh;
    };

    users.users.remotebuild = {
      isSystemUser = true;
      group = "remotebuild";
      useDefaultShell = true;

      openssh.authorizedKeys.keyFiles = [ ./remotebuild.pub ];
    };

    users.groups.remotebuild = {};

    nix.settings.trustedUsers = [ "remotebuild" ];

    virtualisation.oci-containers.backend = "podman";
    virtualisation.quadlet.autoEscape = true;

    system.stateVersion = "23.11";

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
      localDomain = "klaus.tbr.gg";
      email = "tibor@pilz.berlin";
      tunnelId = "7bc72af5-d729-4084-b8ee-42fb0f6f800a";
      basicAuth = {
        enable = true;
        username = "tibor";
        password = "$2y$05$hchzpHMV8QabeLBTgSjIa.3Nqc7uqblFiQ8WTLKq4xSl4ZmR9rDGu";
      };
    };

    modules.services.reverseProxy.proxies.homeassistant = {
      publicPort = 8123;
      targetHost = "192.168.1.175";
      auth = false;
    };

    modules.services = {
      linkwarden = {
        enable = true;
        envFile = config.sops.secrets.linkwardenEnv.path;
      };
      syncthing.enable = true;
      tandoor.enable = true;
      paperless.enable = true;
      firefly-iii.enable = true;
      monitoring.enable = true;
      monitoring.netdata.enable = true;
      linkding.enable = true;
      authentik.enable = true;
      authentik.envFile = config.sops.secrets.authentikEnv.path;

      nextcloud = {
        enable = false;
        adminpassFile = config.sops.secrets.nextcloud_admin_pass.path;
        dataDir = "/data/nextcloud";
        # home = "/nextcloud";
      };

      media = {
        deluge = {
          enable = true;
          credentialsFile = config.sops.secrets.deluge.path;
        };
        sonarr.enable = true; # search & download tv shows
        radarr.enable = true; # search & download movies
        readarr.enable = true; # search & download books
        jackett.enable = true; # indexer for media
        flaresolverr.enable = true;
        pinchflat.enable = true;

        komga.enable = true; # comic reader
        calibre.enable = true; # book reader

        immich = {
          immich-version = "v1.132.3";
          enable = true;
        };
        jellyfin.enable = true;
      };
    };

    modules.services.penpot = {
      enable = false;
      dataDir = "/data/penpot";
    };

    modules.services.woodpecker = {
      enable = false;
      envFile = config.sops.secrets.woodpeckerEnv.path;
    };

    modules.services.forgejo.enable = true;

    services.k3s.enable = false;
    services.k3s.role = "server";
    services.k3s.extraFlags = [ ]; # None for now

    services.radicle-explorer.enable = false;
    services.radicle.enable = false;

    services.vikunja = {
      enable = true;
      port = 3456; # Default, but let's be explicit
      frontendScheme = "https";
      frontendHostname = "vikunja.tiborpilz.xyz";
    };

    modules.services.reverseProxy.proxies.vikunja = {
      publicPort = 3456;
      auth = false;
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
