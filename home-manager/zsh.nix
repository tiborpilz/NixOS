{ config, pkgs, ... }:
{
  programs.zsh = {
    enable = true;

    prezto = {
      enable = true;

      editor.keymap = "vi";
      terminal = {
        autoTitle = true;
      };
    };
  };
}
