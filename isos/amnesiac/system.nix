# Read-only squashfs with a tmpfs overlay, so every write is gone at power-off.
# Traffic goes through Tor (./tor.nix); the only thing surviving a reboot is an
# optional LUKS partition on the same stick (./persistence.nix).
{ lib, pkgs, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-base.nix")
    ./tor.nix
    ./persistence.nix
  ];

  # A distinctive hostname is a fingerprint the local network sees; "amnesia" is
  # what Tails uses, so match the crowd.
  networking.hostName = lib.mkForce "amnesia";

  image.baseName = lib.mkForce "amnesiac";
  isoImage.volumeID = lib.mkForce "AMNESIAC"; # max 32 chars

  # Renaming installation-device.nix's `nixos` user keeps its autologin/sudo
  # wiring intact, rather than adding a second, half-configured account.
  users.users.nixos = {
    name = lib.mkForce "amnesia";
    description = lib.mkForce "Amnesia";
    uid = lib.mkForce 1000; # persistence.nix chowns to this
    extraGroups = [ "wheel" "networkmanager" "video" "audio" "input" ];
  };
  services.getty.autologinUser = lib.mkForce "amnesia";

  # The account has no password, so PAM must accept an empty one or a locked
  # screen is unrecoverable. Run `passwd` after boot to make the lock mean
  # something.
  security.pam.services.xscreensaver.allowNullPassword = true;
  security.pam.services.sddm.allowNullPassword = true;

  services.xserver.enable = true;
  services.xserver.desktopManager.lxqt.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.autoLogin = {
    enable = true;
    user = "amnesia";
  };

  # LXQt ships xscreensaver as a package but not the service that gives it a
  # setuid helper and a PAM stack, without which the locker cannot authenticate.
  services.xscreensaver.enable = true;

  # On an anonymity system an SSH server is a way in, not a feature.
  services.openssh.enable = lib.mkForce false;

  networking.wireless.enable = lib.mkForce false;
  networking.networkmanager = {
    enable = true;
    # A stable MAC identifies the stick to every network it ever touches.
    wifi.macAddress = "random";
    wifi.scanRandMacAddress = true;
    ethernet.macAddress = "random";
    # NM's connectivity check phones a NixOS-hosted URL outside Tor on every
    # link change.
    settings.connectivity.enabled = false;
    dns = "none"; # tor.nix owns resolv.conf
  };

  hardware.enableRedistributableFirmware = true;
  hardware.bluetooth.enable = false;

  # Installer ballast: the manual and a prebuilt stdenv for offline
  # `nixos-install`. Both are mkImageMediaOverride, hence mkForce.
  documentation.nixos.enable = lib.mkForce false;
  system.extraDependencies = lib.mkForce [ ];

  # Nothing may reach a disk; swap in particular would spill keys onto a medium
  # that outlives the session.
  swapDevices = lib.mkForce [ ];
  zramSwap.enable = lib.mkForce false;
  services.journald.extraConfig = "Storage=volatile";
  boot.tmp.useTmpfs = true;

  # Overwriting freed pages shrinks the cold-boot window. Not a substitute for
  # Tails' RAM wipe on shutdown, which has no NixOS equivalent.
  boot.kernelParams = [ "page_poison=1" "slub_debug=P" ];

  # Tor has no transparent-proxy story for IPv6, so any v6-capable path is a
  # leak around tor.nix.
  boot.kernel.sysctl = {
    "net.ipv6.conf.all.disable_ipv6" = 1;
    "net.ipv6.conf.default.disable_ipv6" = 1;
  };

  # VeraCrypt's TrueCrypt licence makes nixpkgs mark it unfree. Scoped to the
  # one package rather than blanket allowUnfree.
  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [ "veracrypt" ];

  environment.systemPackages = with pkgs; [
    # anonymity / crypto
    tor-browser
    onionshare-gui
    keepassxc
    gnupg
    kdePackages.kleopatra # the only KF6 pull left; no Qt-native equivalent
    pinentry-qt
    age
    cryptsetup
    veracrypt
    mat2 # strips document/image metadata
    openssl

    # comms
    thunderbird

    # everyday. LXQt already supplies the file manager, terminal, archiver and
    # image viewer (pcmanfm-qt, qterminal, lxqt-archiver, lximage-qt).
    firefox # fallback; still forced through Tor by nftables
    libreoffice-qt
    qpdfview
    gimp
    vlc
    featherpad

    # tools
    gparted
    git
    wget
    curl
    file
    usbutils
    pciutils
  ];

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-qt;
  };

  services.getty.helpLine = lib.mkAfter ''

    Amnesiac -- amnesic Tor live system.
      * Everything is forgotten at power-off.
      * `persist-setup`  creates the encrypted partition on this USB stick.
      * The unlock prompt appears on tty1 before the desktop starts.
      * If Tor will not bootstrap, check the clock: `sudo date -s "..."`.
  '';

  system.stateVersion = "26.05";
}
