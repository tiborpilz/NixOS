{ pkgs, lib, ... }:
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

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "homeserver";
    time.timeZone = "Europe/Berlin";

    networking.useDHCP = false;
    networking.interfaces.enp3s0.useDHCP = false;
    networking.interfaces.wlp4s0.useDHCP = false;

    networking.interfaces.wlp4s0.ipv4.addresses = [{
      address = "192.168.2.68";
      prefixLength = 24;
    }];

    networking.wg-quick.interfaces = {
      wg0 = {
        address = [ "10.0.0.2/24" "fdc9:281f:04d7:9ee9::2/64" ];
        dns = [ "10.0.0.1" "fdc9:281f:04d7:9ee9::1" ];
        privateKeyFile = "/var/lib/wireguard/private.key";

        peers = [
          {
            publicKey = "QzJm9puVez50UZbCUAJYZnqBdW19o1tBU0Q/WXZsbyw=";
            allowedIPs = [ "0.0.0.0/0" "::/0" ];
            endpoint = "159.69.194.44:51820";
            persistentKeepalive = 25;
          }
        ];
      };
    };

    # networking.useNetworkd = false;

    networking.firewall.enable = false;

    networking.networkmanager.enable = true;

    i18n.defaultLocale = "en_US.UTF-8";

    services.logind.lidSwitch = "ignore";

    fileSystems = {
      "/".label = "nixos-root";
    };

    services.qemuGuest.enable = true;

    services.openssh.enable = true;
    services.openssh.permitRootLogin = "yes";

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
    ];

    users.extraUsers.root.password = "";
    users.mutableUsers = false;

    users.users.tibor = {
      uid = 1000;
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      password = "password";
      shell = pkgs.zsh;
    };

    virtualisation.oci-containers.backend = "podman";
    system.stateVersion = "22.11";
    nixpkgs.config.allowUnfree = true;

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

    home.enable = true;

    modules.services.reverseProxy = {
      enable = true;
      hostname = "tiborpilz.xyz";
      email = "tibor@pilz.berlin";
      basicAuth = {
        enable = true;
        username = "tibor";
        password = "$2y$05$hchzpHMV8QabeLBTgSjIa.3Nqc7uqblFiQ8WTLKq4xSl4ZmR9rDGu";
      };
    };

    modules.services.syncthing.enable = true;
    modules.services.tandoor.enable = true;
    modules.services.paperless.enable = true;
    modules.services.firefly-iii.enable = true;

    modules.services.media = {
      calibre.enable = true;
      deluge.enable = true;
      komga.enable = true;
      plex.enable = true;
    };
  };
}
