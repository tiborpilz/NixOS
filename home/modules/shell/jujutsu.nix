{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.jujutsu;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.shell.jujutsu = with types; {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.jujutsu
      unstable.jjui
      unstable.jujuutils
    ];

    modules.shell.zsh.fpathDirs = "${pkgs.unstable.jujutsu}/share/zsh/site-functions";
  };
}
