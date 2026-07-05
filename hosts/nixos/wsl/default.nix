{ inputs, pkgs, ... }:

# NixOS running inside WSL2 (Windows Subsystem for Linux).
#
# Unlike the bare-metal hosts, this one has no hardware-configuration.nix,
# no bootloader and no display server: WSL provides the kernel and Windows
# handles the GUI. The NixOS-WSL module wires up the interop bits (systemd,
# /mnt/c mounts, the `wsl.exe` shim, Windows PATH integration, ...).
#
# Build a rootfs tarball on any Nix machine and import it into Windows:
#
#   nix build .#nixosConfigurations.wsl.config.system.build.tarballBuilder
#   sudo ./result/bin/nixos-wsl-tarball-builder      # -> nixos.wsl
#   # then, in Windows:
#   wsl --install --from-file nixos.wsl
#
# Afterwards, rebuild from inside WSL with:
#
#   sudo nixos-rebuild switch --flake .#wsl

{
  imports = [ inputs.nixos-wsl.nixosModules.default ];

  config = {
    wsl = {
      enable = true;
      defaultUser = "tibor";
      # Register `.desktop` entries with the Windows start menu.
      startMenuLaunchers = true;
    };

    networking.hostName = "wsl";

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

    programs.zsh.enable = true;

    users.users.tibor = {
      isNormalUser = true;
      description = "Tibor Pilz";
      extraGroups = [ "wheel" ];
      shell = pkgs.zsh;
    };

    # Home Manager is wired up by modules/nixos/home.nix (enabled by default).

    environment.systemPackages = with pkgs; [
      git
      tmux
      vim
      wget
      python3
    ];

    # A container runtime is handy in WSL; podman works without a daemon.
    virtualisation = {
      containers.enable = true;
      podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };

    system.stateVersion = "24.05";
  };
}
