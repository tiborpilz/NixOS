args@{ inputs, pkgs, lib, ... }:

with lib;
with lib.my;

{
  imports = mapModulesRec' (toString ./modules) import;

  home.username = "tibor";
  home.homeDirectory = "/home/tibor";

  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.man.enable = false;

  modules.shell.zsh.enable = true;
  modules.shell.tmux.enable = true;
  modules.shell.gnupg.enable = true;
  modules.shell.git.enable = true;
}
