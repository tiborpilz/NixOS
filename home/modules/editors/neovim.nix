{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.neovim;
in {
  options.modules.editors.neovim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # editorconfig-core-# c
      neovim
    ];

    xdg.configFile."nvim" = { source = ../../config/neovim; recursive = true; };

    home.activation.installNeovimPlugins = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      ${pkgs.neovim}/bin/nvim +'PlugInstall --sync' +qall
    '';
  };
}
