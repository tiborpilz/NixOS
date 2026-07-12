{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.modules.password-store;
  mylib = import ../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.password-store = {
    enable = mylib.mkBoolOpt false;
    enable-sync = mylib.mkBoolOpt false;
  };

  config.home.packages = mkIf cfg.enable [
    pkgs.pass2csv
    pkgs.bitwarden-cli
    pkgs.my.bw2pass # custom script that imports bitwarden to pass
  ];

  config.programs.password-store = mkIf cfg.enable {
    enable = true;
    # keep the pre-26.05 default since the store already lives here
    settings = { PASSWORD_STORE_DIR = "$XDG_DATA_HOME/password-store"; };
  };
  config.services.password-store-sync = mkIf (cfg.enable && cfg.enable-sync) {
    enable = true;
  };
}
