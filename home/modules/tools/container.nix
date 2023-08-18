{ lib, pkgs, config, inputs, ... }:
with lib;
let
  cfg = config.modules.tools.container;
  mylib = import ../../../lib { inherit inputs lib pkgs; };

  darwinConfig = {
    home.packages = with pkgs; [
      colima

      # necessary for podman
      qemu
    ];
  };

  sharedConfig = {
    home.packages = with pkgs; [
      podman
      podman-compose
    ];
  };
in
{
  options.modules.tools.container = {
    enable = mkEnableOption "container";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf pkgs.stdenv.targetPlatform.isDarwin darwinConfig)
    sharedConfig
  ]);
}
