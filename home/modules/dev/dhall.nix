{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.dhall;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.dhall = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      dhall
      dhall-docs
      dhall-json
      dhall-nix
      dhall-nixpkgs
      dhall-lsp-server
    ];
  };
}
