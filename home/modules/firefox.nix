{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.firefox;
  mylib = import ../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.firefox = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox.override {
        cfg = { enableTridactylNative = true; };
      };
    };
  };
}
