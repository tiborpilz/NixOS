{ config, pkgs, ... }:
{
  time.timeZone = "Europe/Berlin";
  imports = [
    ./workyMcWorkstation.nix
    ./xserver/default.nix
  ];

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
    nixops.thefuck
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

  nix.package = pkgs.nixUnstable;
  nix.allowedUsers = [ "@wheel" ];
  nix.useSandbox = false;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = "20.09";
}
