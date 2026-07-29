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

    # Gluetun-only env file: it must NOT see the legacy PASSWORD variable from
    # the deluge secret, which gluetun treats as an alias for OPENVPN_PASSWORD.
    sops.secrets.gluetun = {
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

    sops.secrets.authentik_grafana_client_id = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "grafana";
    };

    sops.secrets.authentik_grafana_client_secret = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "grafana";
    };

    sops.secrets.grafana_secret_key = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "grafana";
    };

    sops.secrets.linkwardenEnv = {
      sopsFile = ./secrets/secrets.yaml;
    };

    # Env file for the LiteLLM voice gateway: DEEPSEEK_API_KEY + LITELLM_MASTER_KEY.
    sops.secrets.litellmEnv = {
      sopsFile = ./secrets/secrets.yaml;
    };

    sops.secrets.woodpeckerEnv = {
      sopsFile = ./secrets/secrets.yaml;
      owner = "woodpecker";
    };

    sops.secrets.tailscale_auth_key = {
      sopsFile = ./secrets/secrets.yaml;
      mode = "0400";
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

    powerManagement.enable = true;
    powerManagement.cpuFreqGovernor = "powersave";
    powerManagement.cpufreq.max = 1200000; # Force low speeds so the boi doesn't get hot
    powerManagement.cpufreq.min = 400000;

    networking.hostName = "klaus";
    networking.hostId = "a5fdeadb";

    time.timeZone = "Europe/Berlin";


    networking.useDHCP = false;
    networking.firewall.enable = false;
    networking.networkmanager.enable = true;
    networking.networkmanager.plugins = lib.mkForce [];


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
      # GTX 1080 (Pascal) was dropped from the 595+ driver series; it needs the 580.xx legacy branch
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };

    hardware.nvidia-container-toolkit.enable = true;

    i18n.defaultLocale = "en_US.UTF-8";

    services.zfs.autoScrub.enable = true;

    services.nfs.server.enable = true;

    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "yes";

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
      netclient
    ];

    systemd.services.netclient = {
      description = "Netmaker client daemon";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.netclient}/bin/netclient daemon";
        Restart = "on-failure";
        StateDirectory = "netclient";
      };
    };

    # Tailscale gives CI access to Klaus that is independent of the
    # Cloudflare Tunnel.
    services.tailscale = {
      enable = true;
      authKeyFile = config.sops.secrets.tailscale_auth_key.path;
    };

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

    # Deploy key for the deploy workflow (private half is the DEPLOY_SSH_KEY
    # secret). See docs/deploy-rs.md.
    users.users.root.openssh.authorizedKeys.keyFiles = [ ./deploy.pub ];

    users.users.remotebuild = {
      isSystemUser = true;
      group = "remotebuild";
      useDefaultShell = true;

      openssh.authorizedKeys.keyFiles = [ ./remotebuild.pub ];
    };

    users.groups.remotebuild = {};

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

      # sshd via the tunnel (ssh.tiborpilz.xyz) for deploy-rs, gated by
      # Cloudflare Access. See docs/deploy-rs.md.
      ssh.enable = true;

      # Uses Cloudflare Tunnel
      # Additionally Secured with Cloudflare Access using authentik as IdP.
      # (Excluded from Cloudflare Access: Homeassistant, Authentik, Forgejo.
      #  Forgejo runs its own OIDC against Authentik, so a CF Access gate in
      #  front would double-auth the browser and block Woodpecker's API
      #  callbacks.)
      tunnelId = "7bc72af5-d729-4084-b8ee-42fb0f6f800a"; # Cloudflare Tunnel
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
      homepage.enable = true;

      # Fully-local voice assistant compute (Whisper/Piper/openWakeWord/Ollama).
      # Home Assistant orchestrates these from 192.168.1.175 over the LAN.
      voice.enable = true;
      # Cloud conversation agent via LiteLLM (DeepSeek primary, Ollama fallback).
      voice.gateway = {
        enable = true;
        environmentFile = config.sops.secrets.litellmEnv.path;
      };

      linkwarden = {
        enable = true;
        envFile = config.sops.secrets.linkwardenEnv.path;
      };
      syncthing.enable = true;
      tandoor.enable = true;
      paperless.enable = true;
      firefly-iii.enable = true;
      monitoring.enable = true;
      monitoring.netdata.enable = false;
      linkding.enable = true;
      excalidraw.enable = true;
      authentik.enable = true;
      authentik.envFile = config.sops.secrets.authentikEnv.path;
      authentik.applications.paperless = {
        displayName = "Paperless";
        redirectUris = [
          "https://paperless.tiborpilz.xyz/accounts/oidc/authentik/login/callback/"
        ];
      };
      authentik.applications.forgejo = {
        displayName = "Forgejo";
        redirectUris = [
          "https://forgejo.tiborpilz.xyz/user/oauth2/authentik/callback"
        ];
      };
      authentik.applications.tandoor = {
        displayName = "Tandoor";
        redirectUris = [
          "https://tandoor.tiborpilz.xyz/accounts/oidc/authentik/login/callback/"
        ];
      };
      authentik.applications.sonarqube = {
        displayName = "SonarQube";
        redirectUris = [
          "https://sonarqube.tiborpilz.xyz/oauth2/callback/oidc"
        ];
      };
      authentik.applications.grafana = {
        displayName = "Grafana";
        redirectUris = [
          "https://grafana.tiborpilz.xyz/login/generic_oauth"
        ];
      };
      # Grants Grafana org Admin via role_attribute_path in the monitoring module.
      authentik.groups."Grafana Admins".members = [ "Tibor" ];

      nextcloud = {
        enable = false;
        adminpassFile = config.sops.secrets.nextcloud_admin_pass.path;
        dataDir = "/data/nextcloud";
        # home = "/nextcloud";
      };

      media = {
        deluge = {
          enable = true;
          credentialsFile = config.sops.secrets.gluetun.path;
        };
        sonarr.enable = true; # search & download tv shows
        radarr.enable = true; # search & download movies
        readarr.enable = true; # search & download books
        jackett.enable = true; # indexer for media
        flaresolverr.enable = true;
        pinchflat.enable = true;

        komga.enable = true; # comic reader
        calibre.enable = true; # book reader

        audiobookshelf = {
          enable = true;
        };

        immich = {
          immich-version = "v3.0.3";
          enable = true;
        };
        jellyfin.enable = true;
        music-assistant.enable = true;
      };
    };

    modules.services.penpot = {
      enable = false;
      dataDir = "/data/penpot";
    };

    modules.services.woodpecker = {
      enable = true;
      envFile = config.sops.secrets.woodpeckerEnv.path;
    };

    modules.services.sonarqube.enable = false;

    modules.services.gitea-sonarqube-bot = {
      enable = false;
      # Add per-repo entries here as projects come online in SonarQube
      # AND you've registered the corresponding webhook in Forgejo.
      projects = [ ];
    };

    modules.services.frigate.enable = true;

    modules.services.forgejo = {
      enable = true;
      sshDomain = "git.tiborpilz.xyz";
    };

    services.k3s.enable = false;
    services.k3s.role = "server";
    services.k3s.extraFlags = [ ]; # None for now

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
