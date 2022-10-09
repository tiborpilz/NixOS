args@{ inputs, pkgs, lib, ... }:

with lib;
with lib.my;

{
  imports = mapModulesRec' (toString ./modules) import;
  #   (
  #     import ./editors/emacs.nix (
  #       args
  #       // { inherit inputs; }
  #     )
  #   )
  #   ./shell/zsh.nix
  # ];

  home.username = "tibor";
  home.homeDirectory = "/home/tibor";

  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.man.enable = false;

  modules.shell.zsh.enable = true;
}
