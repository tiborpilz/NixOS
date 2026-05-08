{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.editors.emacs;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
in
{
  options.modules.editors.emacs = {
    enable = mylib.mkBoolOpt false;
    useNix = mylib.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;
    programs.emacs.extraConfig = ''
      (setenv "LSP_USE_PLISTS" "true")
      (setq lsp-use-plists t)
    '';

    programs.emacs.package = mkIf (!cfg.useNix) pkgs.my.emacsWrapped;

    programs.doom-emacs = mkIf cfg.useNix {
      enable = true;
      doomDir = pkgs.my.doom-emacs-config;
      emacs = pkgs.my.emacsWrapped;
      tangleArgs = "--all config.org";
      extraPackages = epkgs: [
        (epkgs.melpaBuild {
          pname = "copilot";
          version = "0.2.0";
          src = pkgs.fetchFromGitHub {
            owner = "copilot-emacs";
            repo = "copilot.el";
            rev = "v0.2.0";
            sha256 = "sha256-hIA+qdWoOJI9/hqBUSHhmh+jjzDnPiZkIzszCPuQxd0=";
          };
          files = ''(:defaults "dist")'';
          packageRequires = with epkgs; [
            dash
            editorconfig
            f
            jsonrpc
            s
          ];
          propagatedUserEnvPkgs = [ pkgs.nodejs ];
        })
      ];
      extraBinPackages = with pkgs; [
        git
        ripgrep
        fd
        imagemagick
        pinentry-emacs
        zstd
        editorconfig-core-c
        sqlite
        gnuplot
        pandoc
        nodePackages.mermaid-cli
        copilot-language-server
        my.emacs-lsp-booster
      ];
    };

    services.emacs.enable = lib.mkIf (pkgs.stdenv.hostPlatform.isLinux) true;

    home.packages = with pkgs; [
      # emacsWithNativeComp
      libgccjit

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
      # python3Packages.pylint

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

    # home.sessionVariables.DOOMDIR = (if !cfg.useNix then "${config.home.homeDirectory}/.config/nixos/home/config/doom" else "");

    home.activation.installDoomEmacs =
      let
        activationScript = ''
          if [ ! -d ".config/emacs" ]; then
              ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
          fi

          # if [ ! -d ".config/nixos" ]; then
          #     ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/tiborpilz/nixos ".config/nixos"
          # fi

          # if [ ! -d ".config/doom" ]; then
          #    tempdir=$(mktemp -d)
          #    ${pkgs.git}/bin/git clone https://github.com/tiborpilz/nixos $tempdir
          #    cp -r $tempdir/home/config/doom ~/.config/doom
          # fi
          # .config/emacs/bin/doom sync
        '';
      in
      (lib.hm.dag.entryAfter [ "WriteBoundary" ] (if cfg.useNix then "" else activationScript));
  };
}
