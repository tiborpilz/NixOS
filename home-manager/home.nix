{ config, pkgs, ... }:

{
  imports = [
    ./development/default.nix
    ./vim/default.nix
    ./zsh.nix
  ];

  home = {
    username = "tibor";
    homeDirectory = "/home/tibor";
    packages = with pkgs; [
      asciinema
      nushell
      htop
      fzf
      nixpkgs-fmt
      rnix-lsp
      polybar
      firefox
      dmenu
      direnv
    ];
  };

  programs = {
    git = {
      enable = true;
      userName = "Tibor Pilz";
      userEmail = "tibor@pilz.berlin";
    };
  };

  home.stateVersion = "20.09";
}
