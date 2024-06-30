{ pkgs, ... }:

{
  imports = [./hardware-configuration.nix];

  config = {
    # Bootloader.
    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/sda";
    boot.loader.grub.useOSProber = true;

    networking.hostName = "minithink";
    networking.hostId = "3fa82cd1";

    # Enable networking
    networking.networkmanager.enable = true;

    time.timeZone = "Europe/Berlin";

    i18n.defaultLocale = "en_US.utf8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "de_DE.utf8";
      LC_IDENTIFICATION = "de_DE.utf8";
      LC_MEASUREMENT = "de_DE.utf8";
      LC_MONETARY = "de_DE.utf8";
      LC_NAME = "de_DE.utf8";
      LC_NUMERIC = "de_DE.utf8";
      LC_PAPER = "de_DE.utf8";
      LC_TELEPHONE = "de_DE.utf8";
      LC_TIME = "de_DE.utf8";
    };

    # Xserver
    services.xserver = {
      enable = true;

      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;

      # Keyboard
      layout = "us";
      xkbVariant = "";
      xkbOptions = "caps:swapescape";

      libinput = {
        enable = true;

        mouse = {
          accelProfile = "flat";
        };

        touchpad = {
          accelProfile = "flat";
        };
      };
    };

    # Enable Smartcard support
    hardware.gpgSmartcards.enable = true;
    services.pcscd.enable = true;

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable sound with pipewire.
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    programs.zsh.enable = true;

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
    home.enable = true;

    # modules.services.paperless.enable = true;

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

    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };

    nix.settings.experimental-features = [ "nix-command" "flakes" ];
    nix.settings.system-features = [ "big-parallel" "kvm" "recursive-nix" ];
    nix.package = pkgs.nix;
    system.stateVersion = "22.05";
  };
}
