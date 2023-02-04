{ inputs, pkgs, lib, config, ... }:

with lib;

let mylib = import ../lib { inherit inputs lib pkgs; };
in 
  with mylib;
  {
  imports = mylib.mapModulesRec' (toString ./modules) import;

  home.stateVersion = "22.11";

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    etBook

    # Need later version of bash for nix-shell to work correctly on macos
    bash

    # Setuptools is missing from python
    python3Packages.setuptools

    # bat is a better cat (as a program, at least)
    bat

    htop

    pandoc
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Regardlass of whether I'm using Bash (I'm not),
  # I need an up-to-date binary for nix-shell and some other settings in ".profile" that
  # are only there when `bash` is enabled.

  programs.bash.enable = true;

  programs.man.enable = true;

  modules.shell.zsh.enable = true;
  modules.shell.zsh.aliases.ungron = "gron --ungron";

  modules.shell.tmux.enable = true;
  modules.shell.gnupg.enable = false;
  modules.shell.git.enable = true;
  modules.shell.direnv.enable = true;

  programs.alacritty.enable = true;

  modules.editors.neovim.enable = true;
  modules.editors.emacs.enable = true;
  modules.editors.emacs.useNix = false;

  modules.tools.vagrant.enable = false;

  modules.syncthing.enable = true;

  modules.bitwarden.enable = false;
  modules.password-store.enable = true;

  modules.colorschemes.enable = false;

  modules.tools.container.enable = true;

  nix.settings = {
    build-users-group = "nixbld";
    experimental-features = [ "nix-command flakes" ];
    cores = 0;
    max-jobs = "16";
    trusted-users = [ "root" "tibor" ];
    # trusted-substituters = [ "https://cache.nixos.org/" "https://tiborpilz.cachix.org/" ];
    substituters = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org/"
      "https://tiborpilz.cachix.org/"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "tiborpilz.cachix.org-1:KyBjAXY8eblxntQ+OG13IjT+M222VxT+25yw1lqnQS4="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    system-features = [ "big-parallel" "kvm" "recursive-nix" ];
  };

  # Copy Nix-installed MacOS applications to the home application folder, while resolving symlinks
  # This is due to spotlight not resolving symlinks for some reason
  # TODO: check if this will still work with a nix-managed doom config
}
