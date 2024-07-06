{ inputs, pkgs, lib, config, ... }:

with lib;

let
  mylib = import ../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  imports = mylib.mapModulesRec' (toString ./modules) import;

  options.graphical = mkBoolOpt true;

  config = {
    home.stateVersion = "23.11";

    fonts = {
      fontconfig.enable = true;
    };

    home.packages = with pkgs; [
      # TODO: move fonts to own module
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
      etBook
      dejavu_fonts

      # Need later version of bash for nix-shell to work correctly on macos
      bash

      # Setuptools is missing from python
      python3Packages.setuptools

      # GNU Parallel my beloved
      parallel

      # C stuff
      gnumake
      gcc

      # Task runner
      just

      # bat is a better cat (as a program, at least)
      bat

      # mob.sh - git-powered pair/mob programming tool
      mob

      htop

      pandoc

      # PHP & composer
      php82
      php82Packages.composer

      # Bun is a Node alternative
      unstable.bun

      esbuild

      # Latex stuff TODO: move to a module
      # texlive.combined.scheme-full
      # GNU roff - typesetting, pdf converting stuff
      groff

      # Haskell
      cabal-install
      ghc
    ];

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Regardlass of whether I'm using Bash (I'm not),
    # I need an up-to-date binary for nix-shell and some other settings in ".profile" that
    # are only there when `bash` is enabled.
    # programs.bash.enable = true;

    programs.man.enable = true;

    modules.scripts.enable = true;

    modules.shell.zsh.enable = true;
    modules.shell.zsh.aliases.ungron = "gron --ungron";

    modules.shell.tmux.enable = true;
    modules.shell.gnupg = {
      enable = true;
      public_key = "03746612698994281D322B09923BC5E9B4E9509B";
      keygrip = "1050A7CD50EAFCD36E696470775BC39D6FFA47A4";
    };

    modules.shell.git.enable = true;
    modules.shell.direnv.enable = true;

    modules.editors.neovim.enable = true;
    modules.editors.emacs.enable = true;
    modules.editors.emacs.useNix = false;

    modules.dev.rust.enable = true;
    modules.dev.web.enable = true;
    modules.dev.jsonnet.enable = true;
    modules.dev.dhall.enable = true;
    modules.dev.cloud.enable = true;

    modules.dev.colima.enable = pkgs.stdenv.isDarwin; # I only need a docker runtime on MacOs

    # Bit of a catch-all for LSP stuff until I find a better spot
    # without having to create a new module for every one
    modules.editors.lsp.enable = true;

    modules.tools.vagrant.enable = false;
    modules.tools.podman.enable = true;

    modules.syncthing.enable = true;

    modules.bitwarden.enable = false;
    modules.password-store.enable = true;

    modules.tools.container.enable = true;
    modules.tools.aws.enable = true;

    modules.terminal.kitty.enable = true;

    modules.shell.manix.enable = true;

    modules.gui.plasma.enable = config.graphical;

    nix = {
      registry.nixpkgs.flake = inputs.nixpkgs;
      settings = {
        build-users-group = "nixbld";
        experimental-features = [ "nix-command flakes" ];
        cores = 0;
        max-jobs = "32";
        trusted-users = [ "root" "tibor" "tibor.pilz" ];
        trusted-substituters = [ "https://cache.nixos.org/" "https://tiborpilz.cachix.org/" ];
        substituters = [
          "https://cache.nixos.org/"
          "https://nix-community.cachix.org/"
          "https://tiborpilz.cachix.org/"
          "https://cache.garnix.io"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "tiborpilz.cachix.org-1:KyBjAXY8eblxntQ+OG13IjT+M222VxT+25yw1lqnQS4="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        ];
        system-features = [ "big-parallel" "kvm" "recursive-nix" ];
      };
    };
  };
}
