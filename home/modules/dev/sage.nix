{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.sage;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.sage = {
    enable = mylib.mkBoolOpt false;
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      sage
    ];
  };
}

