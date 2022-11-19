args@{ inputs, pkgs, lib, ... }:

with lib;
with lib.my;

let mylib = import ../lib { inherit inputs lib pkgs; };
in {
  imports = mylib.mapModulesRec' (toString ./modules) import;

  home.stateVersion = "22.11";

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.man.enable = false;

  modules.shell.zsh.enable = true;
  modules.shell.zsh.aliases.ungron = "gron --ungron";

  modules.shell.tmux.enable = true;
  modules.shell.gnupg.enable = true;
  modules.shell.git.enable = true;
  modules.shell.direnv.enable = true;

  modules.editors.neovim.enable = true;

  modules.syncthing.enable = true;

  # Install MacOS applications to the user environment if the targetPlatform is Darwin
  # home.file."Applications/home-manager".source = let
  # apps = pkgs.buildEnv {
  #   name = "home-manager-applications";
  #   paths = config.home.packages;
  #   pathsToLink = "/Applications";
  # };
  # in mkIf pkgs.stdenv.targetPlatform.isDarwin "${apps}/Applications";
}
