{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.editors.emacs;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.editors.emacs = {
    enable = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      my.doom-emacs

      fd # for projectile
      imagemagick # for image-dired
      pinentry-emacs # in-emacs gnupg-prompts
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

      # :tools editorconfig
      editorconfig-core-c

      # :tools lookup & :lang org +roam
      sqlite

      # :lang latex & :lang org (late previews)
      (pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-full
          dvisvgm dvipng# preview and export as html
          wrapfig amsmath ulem hyperref capt-of etoolbox titlesec;
      })

      # Haskell language server
      haskell-language-server

      # TODO: move python stuff into dev/python.nix or conditionally enable here
      # Python Language Server
      python3Packages.python-lsp-server

      # Python Language Server Dependencies
      # python3Packages.rope
      python3Packages.pyflakes
      python3Packages.mccabe
      python3Packages.pycodestyle
      python3Packages.pydocstyle
      python3Packages.autopep8
      python3Packages.yapf
      python3Packages.flake8
      python3Packages.pylint

      # Python Language Server Plugins
      python3Packages.pylsp-mypy
      python3Packages.pyls-isort
      python3Packages.python-lsp-black

      # Terraform language server
      terraform-ls

      # Fonts
      emacs-all-the-icons-fonts

      # Markdown conversion and live preview
      pandoc

      # Mermaid diagramming
      nodePackages.mermaid-cli

      # Copilot
      github-copilot-cli

      # doom emacs org :jupyter and :gnuplot
      gnuplot

      # Speed up LSP
      my.emacs-lsp-booster
    ];

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    xdg.configFile."doom" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/nixos/home/config/doom";
      recursive = true;
    };
  };
}
