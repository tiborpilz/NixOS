{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.rust;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.rust = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    # Packages for rust development
    home.packages = with pkgs; [
      rustup
    ];
  };
}
