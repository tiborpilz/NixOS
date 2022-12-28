{ config, options, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.bitwarden;
  mylib = import ../../lib { inherit inputs pkgs lib; };
in {
  options.modules.bitwarden = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bitwarden
      bitwarden-cli
    ];
  };
}
