{ pkgs, lib, config, modulesPath, ... }:
with lib;
{
  imports = [
    ./hardware-configuration.nix
    ./modules/reverseProxy.nix
    # (modulesPath + "/nixos/modules/profiles/qemu-guest.nix")
    # (modulesPath + "/nixos/modules/virtualisation/qemu-vm.nix")
    ./services/tandoor.nix
    # ./services/openhab.nix
    ./services/paperless-ng.nix
    # ./services/homeassistant.nix
    ./services/media/media.nix
    ./services/samba.nix
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
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    services.logind.lidSwitch = "ignore";

    services.avahi = {
      nssmdns = true;
      enable = true;
      ipv4 = true;
      ipv6 = true;
      publish = {
        enable = true;
        addresses = true;
        domain = true;
        hinfo = true;
        userServices = true;
        workstation = true;
      };
    };

    fileSystems = {
      "/".label = "nixos-root";
    };

    services.qemuGuest.enable = true;

    services.openssh.enable = true;
    services.openssh.permitRootLogin = "yes";

    environment.systemPackages = with pkgs; [
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
    };

    virtualisation.oci-containers.backend = "podman";
    system.stateVersion = "22.05";
    nixpkgs.config.allowUnfree = true;

    # services.caddy = {
    #   enable = true;
    #   email = "tibor@pilz.berlin";
    # };

    reverseProxy = {
      hostname = "tiborpilz.xyz";
      email = "tibor@pilz.berlin";
    };
  };
}
