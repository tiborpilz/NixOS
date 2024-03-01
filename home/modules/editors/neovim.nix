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
      neovim
    ];

    modules.shell.zsh.aliases = {
      vim = "nvim";
    };

    xdg.configFile."nvim" = { source = ../../config/neovim; recursive = true; };
  };
}
