{ config, pkgs, ...}:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

in {
  home.packages = with pkgs; [
    direnv
    git
    gnupg
    silver-searcher
  ];

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "Tibor Pilz";
    userEmail = "tibor@pilz.berlin";
  };

  programs.firefox.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;

    oh-my-zsh = {
      enable = true;
      theme = "robbyrussell";
    };
  };

  programs.tmux = {
    enable = true;
  };

  home.file.".tmux.conf" = {
    source = ./dotfiles/tmux/.tmux.conf;
  };

  programs.neovim = {
    enable = true;
  };

  xdg.configFile.nvim = {
    source = ./dotfiles/neovim/.config/nvim;
    recursive = true;
  };
  
  /* xdg.configFile."nvim/init.vim".source = ./dotfiles/neovim/.config/nvim/init.vim; */
  /* xdg.configFile."nvim/look-and-feel.vim".source = ./dotfiles/neovim/.config/nvim/look-and-feel.vim; */
  /* xdg.configFile."nvim/plugins.vim".source = ./dotfiles/neovim/.config/nvim/plugins.vim; */
  /* xdg.configFile."nvim/autoload/plug.vim".source = ./dotfiles/neovim/.config/nvim/autoload/plug.vim; */
}
