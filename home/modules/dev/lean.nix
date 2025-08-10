{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.lean;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.lean = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # lean4
      elan
    ];
  };
}
