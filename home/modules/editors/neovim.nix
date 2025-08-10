{ inputs, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.editors.neovim;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.editors.neovim = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.neovim
      unstable.lua52Packages.lua-utils-nvim
    ];

    modules.shell.zsh.aliases = {
      vim = "nvim";
    };

    xdg.configFile."nvim" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/neovim";
      recursive = true;
    };
  };
}
