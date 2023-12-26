{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.cloud;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.cloud = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      kubectl
      # backup management
      velero
      # plugins
      krew
    ];
  };
}