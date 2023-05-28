{ lib, pkgs, config, inputs, ... }:
with lib;
let
  cfg = config.modules.tools.podman;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.tools.podman = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      podman
      podman-tui
      podman-compose
    ];
  };
}
