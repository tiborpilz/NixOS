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
    programs.doom-emacs = mkIf cfg.useNix rec {
      enable = true;
      doomPrivateDir = ../../config/doom;
      doomPackageDir = let
        filteredPath = builtins.path {
          path = doomPrivateDir;
          name = "doom-private-dir-filtered";
          filter = path: type:
            builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
        };
      in pkgs.linkFarm "doom-packages-dir" [
        { name = "init.el"; path = "${filteredPath}/init.el"; }
        { name = "packages.el"; path = "${filteredPath}/packages.el"; }
        { name = "config.el"; path = pkgs.emptyFile; }
      ];
      emacsPackage = pkgs.my.emacsGit;

      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
        emacs = emacsPackage;
      };

      extraPackages = with pkgs; [ nodejs-16_x ];

      extraConfig = ''
        (setenv "LSP_USE_PLISTS" "true")
        (setq lsp-use-plists t)
        (setq copilot-node-executable "${pkgs.nodejs-16_x}/bin/node")
      '';
    };

    home.packages = with pkgs; [
      # emacsWithNativeComp

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
      texlive.combined.scheme-medium

      # alternative lsp server for nix
      nil

      # typescript language server
      nodePackages.typescript-language-server

      # vue language server
      my."@volar/vue-language-server"

      # Python Language Server
      python3Packages.python-lsp-server

      # Python Language Server Dependencies
      python3Packages.rope
      python3Packages.pyflakes
      python3Packages.mccabe
      python3Packages.pycodestyle
      python3Packages.pydocstyle
      python3Packages.autopep8
      python3Packages.yapf
      python3Packages.flake8
      python3Packages.pylint

      # PPython Language Server Plugins
      python3Packages.pylsp-mypy
      python3Packages.pyls-isort
      python3Packages.python-lsp-black

      # Fonts
      emacs-all-the-icons-fonts
      my.emmet-ls
    ] ++ (if cfg.useNix then [] else [ my.emacsGitXwWrapped ] );

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];


    # xdg.configFile."doom" = { source = ../../config/doom; recursive = true; };

    # home.sessionVariables.DOOMDIR = (if !cfg.useNix then "${config.home.homeDirectory}/.config/nixos/home/config/doom" else "");

    home.activation.installDoomEmacs =
      let activationScript = ''
          if [ ! -d ".config/emacs" ]; then
              ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
          fi

          # if [ ! -d ".config/nixos" ]; then
          #     ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/tiborpilz/nixos ".config/nixos"
          # fi

          if [ ! -d ".config/doom" ]; then
              tempdir=$(mktemp -d)
              ${pkgs.git}/bin/git clone https://github.com/tiborpilz/nixos $tempdir
              cp -r $tempdir/home/config/doom ~/.config/doom
          fi
          # .config/emacs/bin/doom sync
        '';
      in (lib.hm.dag.entryAfter ["WriteBoundary"] (if cfg.useNix then "" else activationScript ));
  };
}
