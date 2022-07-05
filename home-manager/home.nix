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
      nerdfonts
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

    tmux = {
      enable = true;
      baseIndex = 1;
      escapeTime = 250;
      historyLimit = 10000;
      keyMode = "vi";
      newSession = true;

      plugins = with pkgs; [
        tmuxPlugins.continuum
        tmuxPlugins.resurrect
        tmuxPlugins.yank
      ];

      tmuxp.enable = true;
    };
  };
  home.stateVersion = "20.09";
}
