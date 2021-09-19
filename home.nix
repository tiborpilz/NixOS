{ config, pkgs, ...}:
{
  home.packages = with pkgs; [
    direnv
    git
    gnupg
    silver-searcher
    stow
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
    clock24 = true;
    keyMode = "vi";
    secureSocket = false;
  };

  programs.neovim = {
    enable = true;
  };
  
  xdg.configFile."nvim/init.vim".source = ./dotfiles/neovim/.config/nvim/init.vim;
}
