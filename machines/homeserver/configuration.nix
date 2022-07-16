{ pkgs, lib, config, modulesPath, ... }:

with lib;
{
  imports = [
    ./hardware-configuration.nix
    # (modulesPath + "/nixos/modules/profiles/qemu-guest.nix")
    # (modulesPath + "/nixos/modules/virtualisation/qemu-vm.nix")
    # ./services/tandoor.nix
    ./services/openhab.nix
    ./services/paperless-ng.nix
    # ./services/homeassistant.nix
    ./services/media/media.nix
  ];

  config = {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "homeserver";
    time.timeZone = "Europe/Berlin";

    networking.useDHCP = false;
    networking.interfaces.enp3s0.useDHCP = false;
    networking.interfaces.wlp4s0.useDHCP = false;
    networking.useNetworkd = false;

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

  };
}
