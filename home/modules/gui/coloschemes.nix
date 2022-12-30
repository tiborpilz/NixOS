{ inputs, config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.colorschemes;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.colorschemes = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.pywal.enable = true;
    home.packages = with pkgs; [ wpgtk ];
  };
}
