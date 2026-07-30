# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  mountainWallpaper =
    "${pkgs.kdePackages.plasma-workspace-wallpapers}/share/wallpapers/Mountain/contents/images/5120x2880.png";

  sddmMountainTheme = pkgs.runCommand "sddm-breeze-mountain" { } ''
    mkdir -p $out/share/sddm/themes
    cp -r ${pkgs.kdePackages.plasma-desktop}/share/sddm/themes/breeze \
      $out/share/sddm/themes/breeze-mountain
    chmod -R u+w $out/share/sddm/themes/breeze-mountain

    conf=$out/share/sddm/themes/breeze-mountain/theme.conf
    grep -q '^background=' $conf
    sed -i "s|^background=.*|background=${mountainWallpaper}|" $conf
  '';
in
{
  imports =
    [./hardware-configuration.nix];

  config = {
    # Bootloader.
    boot = {
      loader.systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };

      loader.efi.canTouchEfiVariables = true;

      plymouth = {
        enable = true;
        theme = "bgrt"; # firmware vendor logo + spinner, Windows-style
      };

      consoleLogLevel = 3;
      initrd.verbose = false;
      kernelParams = [
        "quiet"
        "udev.log_level=3"
        "systemd.show_status=auto"
      ];

      loader.timeout = 0;
    };

    networking.hostName = "thinkyMcThinkpad";
    networking.networkmanager.enable = true;

    time.timeZone = "Europe/Berlin";

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


    services.displayManager.sddm = {
      enable = true;
      theme = "breeze-mountain";
    };
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

    modules.desktop.keyd.enable = true;

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

    # Enable Smartcard support (YubiKey as GPG/SSH key).
    # gnupg's scdaemon.conf uses disable-ccid, so it talks to the card via
    # PC/SC; pcscd must be running for `gpg --card-status` / ssh to work.
    hardware.gpgSmartcards.enable = true;
    services.pcscd.enable = true;

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

    # Snapcast client: makes this laptop a playback target for klaus's Music
    # Assistant / snapserver (192.168.1.51). Runs as a user service and outputs
    # via PipeWire's pulse server, so it mixes like any other app instead of
    # grabbing the sound card exclusively. Only runs while tibor is logged in.
    home-manager.users.tibor.systemd.user.services.snapclient = {
      Unit = {
        Description = "Snapcast client (-> klaus)";
        After = [ "pipewire-pulse.service" ];
        Wants = [ "pipewire-pulse.service" ];
      };
      Service = {
        ExecStart = "${pkgs.snapcast}/bin/snapclient --host 192.168.1.51 --player pulse";
        Restart = "always";
        RestartSec = 5;
      };
      Install.WantedBy = [ "default.target" ];
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

    # Smaller terminal font on the laptop screen
    home-manager.users.tibor.modules.terminal.kitty.fontSize = 12;

    # Firmware updates via LVFS
    services.fwupd.enable = true;

    services.fprintd.enable = true;
    services.fprintd.tod.enable = true;
    services.fprintd.tod.driver = pkgs.libfprint-2-tod1-goodix;

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
      fprintd
      sddmMountainTheme
    ];

    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };

    system.stateVersion = "26.05";
  };
}
