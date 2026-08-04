# couchthink — ThinkPad X240 HTPC running Plasma Bigscreen on a TV.
#
# Haswell HD 4400 has no hardware HEVC decode (H.264/VC-1/MPEG2 only, via i965)
# and HDMI 1.4 caps output at 1080p60, so Jellyfin and Moonlight are both
# steered to H.264 below.
{ config, pkgs, lib, ... }:
let
  klaus = "192.168.1.51"; # LAN, deliberately not via the reverse proxy

  # Dropped from 26.05 with the rest of Plasma 5; unstable has the Plasma 6
  # port. Links plasma-workspace 6.7.1 against a 6.6.5 system, so if the session
  # misbehaves, fall back to plain Plasma 6 from SDDM.
  plasma-bigscreen = pkgs.unstable.kdePackages.plasma-bigscreen;
in
{
  imports = [
    ./hardware-configuration.nix
    ./disko.nix
  ];

  config = {
    boot = {
      loader.systemd-boot = {
        enable = true;
        configurationLimit = 10;
      };
      loader.efi.canTouchEfiVariables = true;
      loader.timeout = 0;

      # Appliance boot: no text scroll on the TV.
      plymouth.enable = true;
      consoleLogLevel = 3;
      initrd.verbose = false;
      kernelParams = [ "quiet" "udev.log_level=3" ];
    };

    networking.hostName = "couchthink";
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


    hardware.graphics = {
      enable = true;
      enable32Bit = true; # Steam
      extraPackages = with pkgs; [
        # iHD covers Gen8+ only, so Haswell needs i965.
        intel-vaapi-driver
        libva-vdpau-driver
        libvdpau-va-gl
      ];
    };

    environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";


    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };

    # Debugging fallback, and pulls in the KDE plumbing Bigscreen expects.
    services.desktopManager.plasma6.enable = true;

    # passthru.providedSessions on the package registers the session name.
    services.displayManager.sessionPackages = [ plasma-bigscreen ];
    services.displayManager.defaultSession = "plasma-bigscreen-wayland";

    services.displayManager.autoLogin = {
      enable = true;
      user = "media";
    };

    # Bigscreen prepends to XDG_CONFIG_DIRS, so /etc/xdg still applies.
    environment.etc = {
      # A locked TV with no keyboard attached is a brick.
      "xdg/kscreenlockerrc".text = ''
        [Daemon]
        Autolock=false
        LockOnResume=false
      '';
      # Else the first-run wizard pops a modal over the Bigscreen shell.
      "xdg/kwalletrc".text = ''
        [Wallet]
        Enabled=false
        First Use=false
      '';
    };

    # Players inhibit idle while playing; nothing does while you're browsing.
    systemd.targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };

    # The lid lives closed under the TV.
    services.logind.settings.Login = {
      HandleLidSwitch = "ignore";
      HandleLidSwitchExternalPower = "ignore";
      HandleLidSwitchDocked = "ignore";
      HandlePowerKey = "poweroff";
      IdleAction = "ignore";
    };


    # USB keyboard/trackpad combos need nothing; these cover gamepad + phone.
    # programs.steam pulls in hardware.steam-hardware (pad udev rules).
    hardware.xpadneo.enable = true; # Xbox controllers over Bluetooth

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    # Phone as a remote. The module opens 1714-1764 TCP/UDP itself.
    programs.kdeconnect.enable = true;

    services.libinput.enable = true;

    services.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # mDNS, so Music Assistant on klaus can see this box and vice versa.
    # nssmdns4 resolves .local names; publish announces couchthink.local so the
    # box is reachable without hunting for its DHCP lease.
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
      };
    };

    systemd.user.services.snapclient = {
      description = "Snapcast client";
      unitConfig.ConditionUser = "media";
      after = [ "pipewire-pulse.service" ];
      wants = [ "pipewire-pulse.service" ];
      serviceConfig = {
        ExecStart = "${pkgs.snapcast}/bin/snapclient --host ${klaus} --player pulse";
        Restart = "always";
        RestartSec = 5;
      };
      wantedBy = [ "default.target" ];
    };

    systemd.user.services.librespot = {
      description = "librespot (Spotify Connect endpoint)";
      unitConfig.ConditionUser = "media";
      after = [ "pipewire-pulse.service" "network-online.target" ];
      wants = [ "pipewire-pulse.service" ];
      serviceConfig = {
        ExecStart = lib.concatStringsSep " " [
          "${pkgs.librespot}/bin/librespot"
          "--name Couch"
          "--device-type tv"
          "--backend pulseaudio"
          "--bitrate 320"
          "--enable-volume-normalisation"
          "--cache %S/librespot"
          "--zeroconf-port 45301"
        ];
        StateDirectory = "librespot";
        Restart = "always";
        RestartSec = 5;
      };
      wantedBy = [ "default.target" ];
    };

    # librespot discovers over mDNS but pairs over plain HTTP on this port.
    networking.firewall.allowedTCPPorts = [ 45301 ];


    # Big Picture is the only couch-usable part; local gaming on HD 4400 isn't
    # the point. remotePlay opens 27031-27036 for streaming from the gaming PC.
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
    };

    services.flatpak.enable = true;

    # Flatpak with no remote configured can't install anything, and `flatpak
    # remote-add` is imperative state that wouldn't survive a reinstall.
    systemd.services.flathub-remote = {
      description = "Register the Flathub remote";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.flatpak}/bin/flatpak remote-add --if-not-exists \
          flathub https://dl.flathub.org/repo/flathub.flatpakrepo
      '';
    };

    programs.zsh.enable = true;

    # Autologin account, deliberately not in wheel.
    users.users.media = {
      isNormalUser = true;
      description = "Media";
      extraGroups = [ "audio" "video" "input" "networkmanager" ];
      initialHashedPassword = "$y$j9T$Fz9mqs6YQGznhp4n1iGos.$YjDlvnUR0su9gcjLNeBMccHJrQCftFyswBONlEN2kkA"; # test
    };

    # Admin account, SSH only.
    users.users.tibor = {
      isNormalUser = true;
      description = "Tibor Pilz";
      extraGroups = [ "wheel" "networkmanager" ];
      shell = pkgs.zsh;
      initialHashedPassword = "$y$j9T$Fz9mqs6YQGznhp4n1iGos.$YjDlvnUR0su9gcjLNeBMccHJrQCftFyswBONlEN2kkA"; # test
    };

    # ~/.ssh/id — the same key that deploys klaus.
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCVvFqXULJLQqeG10IJY2c0++ypT7iseLqFwGnwUBER tibor@pilz.berlin"
    ];
    users.users.tibor.openssh.authorizedKeys.keys =
      config.users.users.root.openssh.authorizedKeys.keys;

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    services.tailscale = {
      enable = true;
      useRoutingFeatures = "client";
      openFirewall = true;
    };

    # The ~/home config is a dev workstation; it would only fight Bigscreen.
    home.enable = false;

    # Bigscreen's launcher reads .desktop entries, so these become TV tiles.

    environment.systemPackages = with pkgs; [
      plasma-bigscreen

      # FIRST RUN: Settings -> Video, turn HEVC/AV1/VP9 OFF and cap at 1080p,
      # so klaus transcodes to H.264 (free on its GTX 1080) instead of this
      # box software-decoding HEVC.
      jellyfin-media-player

      # FIRST RUN: set the codec to H.264, not "Automatic" — automatic
      # negotiates HEVC and then software-decodes it.
      moonlight-qt

      libva-utils # vainfo: expect i965 with H264 and no HEVC entrypoints

      git
      vim
      htop
      pciutils
      usbutils
    ];

    system.stateVersion = "26.05";
  };
}
