{ inputs, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.shell.zsh;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.zoxide = with types; {
    enable = lib.mkBoolOpt false;
  };

  config = mkfIf cfg.enable {
    modules.shell.zsh.fpathDirs = "${pkgs.zoxide}/share/zsh/site-functions";
    home.packages = [ pkgs.zoxide pkgs.fzf ];
  };
}
