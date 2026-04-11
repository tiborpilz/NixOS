{ pkgs ? import <nixpkgs> { } }:

let
  isDarwin = pkgs.stdenv.isDarwin;

  screenshotDeps = with pkgs; [
    vhs
    ttyd
    ffmpeg
  ] ++ pkgs.lib.optionals (!isDarwin) [
    chromium
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
in
pkgs.mkShell {
  buildInputs = commonDeps ++ screenshotDeps;

  packages = commonDeps ++ [ pkgs.home-manager ] ++ screenshotDeps;

  shellHook = ''
    export FLAKE="$PWD"
    export NH_FLAKE="$PWD"
    export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
  '' + pkgs.lib.optionalString isDarwin ''
    # VHS needs a Chrome-compatible browser; use system Chrome on macOS
    export CHROME_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  '';
}

