{ config, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    enableVteIntegration = true;
    defaultKeymap = "vicmd";

    zplug = {
      enable = true;

      plugins = [
        { name = "zsh-users/zaw"; }
        { name = "junkblocker/calibre-zaw-source"; }
      ];
    };
  };
}
