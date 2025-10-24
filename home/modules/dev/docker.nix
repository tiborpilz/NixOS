{ inputs, config, lib, pkgs, ... }:
let
  cfg = config.modules.dev.docker;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.dev.docker = {
    enable = mylib.mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      docker-compose
      docker
    ];

    modules.shell.zsh.fpathDirs = ''
      ${pkgs.docker-compose}/share/zsh/site-functions
      ${pkgs.docker}/share/zsh/site-functions
    '';
  };
}
