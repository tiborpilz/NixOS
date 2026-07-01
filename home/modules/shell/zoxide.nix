{ inputs, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.shell.zoxide;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.zoxide = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.zoxide pkgs.fzf ];
  };
}
