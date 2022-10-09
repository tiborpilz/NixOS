{ config, pkgs, ... }:

{
  time.timeZone = "Europe/Berlin";
  imports = [
    ./hardware-configuration.nix
  ];

  virtualisation.memorySize = 32768;
  virtualisation.cores = 8;
  virtualisation.diskSize = 32768;
  # Use the systemd-boot EFI boot loader.
  boot = {
    consoleLogLevel = 0;
    loader = {
      efi = {
        efiSysMountPoint = "/boot";
      };

      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        version = 2;
        gfxmodeEfi = "1920x1080";
        gfxpayloadEfi = "keep";
      };

    };

    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "amd_iommu=on" ];
  };

  security.polkit.enable = true;

  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.ovmf.enable = true;
    };
  };

  users.extraUsers.root.password = "";
  users.users.tibor = {
    uid = 1000;
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    password = "password";
  };

  console = {
    earlySetup = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    direnv
    wget
    tmux
    hwinfo
  ];

  programs = {
    dconf.enable = true;
    tmux = {
      enable = true;
      newSession = true;
      terminal = "screen-256color";
    };
    zsh = {
      enable = true;
      enableBashCompletion = true;
      enableCompletion = true;
    };
  };


  networking.hostName = "workyMcNixStation";
  networking.useDHCP = true;

  nix.package = pkgs.nixUnstable;
  nix.settings.allowed-users = [ "@wheel" ];
  nix.settings.sandbox = false;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  system.stateVersion = "22.05";

  modules.services.media.deluge.enable = false;
  modules.desktop.bspwm.enable = true;
}
