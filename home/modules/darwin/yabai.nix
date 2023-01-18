{ inputs, config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.darwin.yabai;
  myLib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.darwin = {
    yabai = {
      enable = myLib.mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    # home.packages = with pkgs; [ yabai skhd ];
    # xdg.configFile."yabai" = { source = ../../config/yabai; recursive = true; };
    # xdg.configFile."skhd" = { source = ../../config/skhd; recursive = true; };
  };
}
