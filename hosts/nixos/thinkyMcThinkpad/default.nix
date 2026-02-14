# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [./hardware-configuration.nix];

  config = {
    # Bootloader.
    boot = {
      loader.grub = {
        enable = true;
        device = "/dev/sda";
        useOSProber = true;
        enableCryptodisk=true;
      };

      initrd = {
        secrets = {
          "/crypto_keyfile.bin" = null;
        };

        luks.devices."luks-105a050d-9c7f-466d-b2af-6a18d7e56b81".keyFile = "/crypto_keyfile.bin";
      };
 
      plymouth = {
        enable = true;
        theme = "rings";
        themePackages = with pkgs; [
          (adi1090x-plymouth-themes.override {
            selected_themes = [ "rings" ];
          })
        ];
      };

      consoleLogLevel = 3;
      initrd.verbose = false;
      kernelParams = [
        "quiet"
        "udev.logevel=3"
        "sysemd.show_tatus=auth"
      ];

      loader.timeout = 0;
    };
    # Setup keyfile

    # boot.loader.systemd-boot.enable = true;
    # boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "thinkyMcThinkpad"; # Define your hostname.

    # Enable networking
    networking.networkmanager.enable = true;

    # Set your time zone.
    time.timeZone = "Europe/Berlin";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };


    services.displayManager.sddm.enable = true;
    services.desktopManager.plasma6.enable = true;

    programs.hyprland.enable = true;

    # Configure keymap in X11
    services.xserver = {
      enable = true;

      windowManager.bspwm.enable = true;

      xkb = {
        layout = "us";
        variant = "";
        options = "caps:swapescape";
      };
    };

    services.libinput = {
      enable = true;

      mouse = {
        accelProfile = "flat";
      };

      touchpad = {
        naturalScrolling = true;
        accelProfile = "flat";
      };
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable sound with pipewire.
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    programs.zsh.enable = true;

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.tibor = {
      isNormalUser = true;
      description = "Tibor Pilz";
      extraGroups = [ "networkmanager" "wheel" ];
      shell = pkgs.zsh;
      packages = with pkgs; [
        firefox
      ];
      initialHashedPassword = "$y$j9T$Fz9mqs6YQGznhp4n1iGos.$YjDlvnUR0su9gcjLNeBMccHJrQCftFyswBONlEN2kkA"; # test
    };

    # Use home-manager
    # (./modules/nixos/home.nix)
    home.enable = true;
    home.graphical = true;

    # modules.services.paperless.enable = true;

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      git
      tmux
      vim
      wireguard-tools
      gparted
      hdparm
      python3
    ];

    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };

    system.stateVersion = "23.11";
  };
}
