{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.python;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.python = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    # Packages for rust development
    home.packages = with pkgs; [
      python3
      python3Packages.setuptools
      uv
    ];
  };
}
