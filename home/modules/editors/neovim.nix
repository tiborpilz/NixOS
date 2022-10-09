{ inputs, config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.editors.neovim;
    mylib = import ../../../lib { inherit inputs lib pkgs; };
in {
  options.modules.editors.neovim = {
    enable = mylib.mkBoolOpt false;
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
