{ inputs, pkgs, lib, config, ... }:

with lib;
let
  cfg = config.modules.dev.web;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
with mylib;
{
  options.modules.dev.colima = {
    enable = mkBoolOpt false;
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.unstable.colima ];
    home.sessionVariables.DOCKER_HOST = "unix://$HOME/.config/colima/default/docker.sock";

    modules.shell.zsh.fpathDirs = "${pkgs.colima}/share/zsh/site-functions";
  };
}
