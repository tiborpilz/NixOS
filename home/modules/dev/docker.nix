{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.docker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.docker = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      docker-compose
      docker
    ];
  };
}
