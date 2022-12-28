{ config, options, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.password-store;
  mylib = import ../../lib { inherit inputs lib pkgs; };
in {
  options.modules.password-store = {
    enable = mylib.mkBoolOpt false;
    enable-sync = mylib.mkBoolOpt false;
  };

  config.home.packages = mkIf cfg.enable [
    pkgs.pass2csv
  ];

  config.programs.password-store = mkIf cfg.enable {
    enable = true;
  };
  config.services.password-store-sync = mkIf (cfg.enable && cfg.enable-sync) {
    enable = true;
  };
}
