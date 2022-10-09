args@{ inputs, pkgs, ... }:

{
  imports = [
    (
      import ./editors/emacs.nix (
        args
        // { inherit inputs; }
      )
    )
  ];

  home.username = "tibor";
  home.homeDirectory = "/home/tibor";

  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.man.enable = false;

}
