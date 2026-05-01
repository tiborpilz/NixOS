{ pkgs ? import <nixpkgs> { } }:

let
  isDarwin = pkgs.stdenv.isDarwin;

  screenshotDeps = with pkgs; [
    git
  ] ++ pkgs.lib.optionals (!isDarwin) [
    zsh
    neovim
    tmux
    fzf
    gitmux
    zoxide
    kitty
    xvfb-run
    imagemagick
    xdotool
    mesa
    emacs
    ripgrep
    fd
    gcc
    fira-code
  ];

  commonDeps = with pkgs; [
    git
    nix-zsh-completions
    node2nix
    cachix
    go-task
    deploy-rs
    sops
    nh
    just
    nodejs_22
  ];

  linuxGlHook = pkgs.lib.optionalString (!isDarwin) ''
    # Force software OpenGL and point libGL at nixpkgs Mesa's DRI drivers
    # so Kitty/anything-GL works under Xvfb on non-NixOS Linux.
    export LIBGL_ALWAYS_SOFTWARE=1
    export LIBGL_DRIVERS_PATH="${pkgs.mesa.drivers or pkgs.mesa}/lib/dri"
  '';
in
{
  default = pkgs.mkShell {
    buildInputs = commonDeps ++ screenshotDeps;
    packages = commonDeps ++ [ pkgs.home-manager ] ++ screenshotDeps;
    shellHook = ''
      export FLAKE="$PWD"
      export NH_FLAKE="$PWD"
      export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
    '' + linuxGlHook;
  };

  screenshots = pkgs.mkShell {
    buildInputs = screenshotDeps;
    packages = screenshotDeps;
    shellHook = linuxGlHook;
  };
}
