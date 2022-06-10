{ config, pkgs, ... }:

{
  time.timeZone = "Europe/Berlin";
  imports = [
    ./desktop/default.nix
  ];

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
        efiSupport= true;
        version = 2;
        gfxmodeEfi = "1920x1080";
        gfxpayloadEfi = "keep";
      };

    };

    kernelModules = ["kvm-amd"];
    kernelParams = ["amd_iommu=on"];
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      qemuOvmf = true;
    };
  };

  users = {
    users = {
      root.hashedPassword = "$1$randomsa$.3CDKHCqAQfgg3uJ4Ra600";
      tibor = {
        isNormalUser = true;
        hashedPassword = "$1$randomsa$.3CDKHCqAQfgg3uJ4Ra600";
        home = "/home/tibor";
        createHome = true;
        extraGroups = [
          "wheel"
          "libvirtd"
        ];
        shell = pkgs.zsh;
      };
    };
  };

  console = {
    earlySetup = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # Languages
    nodejs
    ruby
    # Systemwide console tools
    direnv
    wget
    tmux
    hwinfo
  ];

  networking.hostName = "workyMcNixStation";
  networking.useDHCP = true;

  nix.package = pkgs.nixUnstable;
  nix.allowedUsers = [ "@wheel" ];
  nix.useSandbox = false;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  system.stateVersion = "20.09";
}
