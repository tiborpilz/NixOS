{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let nixBin =
      writeShellScriptBin "nix" ''
        ${nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
      '';
in mkShell {
  buildInputs = [
    git
    nix-zsh-completions
  ];
  shellHook = ''
    export FLAKE="$PWD"
    export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  '';
}
