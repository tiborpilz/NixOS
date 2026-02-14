{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [
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

  packages = with pkgs; [
    git
    nix-zsh-completions
    node2nix
    cachix
    go-task
    deploy-rs
    home-manager
    sops
    nh
    just
    nodejs_22
  ];
  shellHook = ''
    export FLAKE="$PWD"
    export NH_FLAKE="$PWD"
    export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
  '';
}
