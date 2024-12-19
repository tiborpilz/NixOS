{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  nixBin =
    writeShellScriptBin "nix" ''
      ${nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
    '';
in
mkShell {
  buildInputs = [
    git
    nix-zsh-completions
    node2nix
    nixos-rebuild
    cachix
    go-task
    deploy-rs
  ];
  shellHook = ''
    export FLAKE="$PWD"
    export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
    export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/local/share:/usr/share
  '';
}
