{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.editors.emacs;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  # Otherwise, emacs can't handle some LSP control characters
  patchedEmacs = pkgs.emacs.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (pkgs.fetchpatch {
        url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
        hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
      })
    ];
  });
  emacsWithNativeComp = patchedEmacs.override {
    nativeComp = false;
  };
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
      emacsPackage = emacsWithNativeComp;

      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
      };
      # package = emacsPackage;

      extraConfig = ''
        (setenv "LSP_USE_PLISTS" "true")
      '';
    };

    home.packages = with pkgs; [
      emacsWithNativeComp

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

      # Fonts
      emacs-all-the-icons-fonts
      my.emmet-ls
    ];

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    home.activation.installDoomEmacs = mkIf (! cfg.useNix) (lib.hm.dag.entryAfter ["WriteBoundary"] ''
        if [ ! -d ".config/emacs" ]; then
            ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
        fi

        if [ ! -d ".config/doom" ]; then
            tempdir=$(mktemp -d)
            ${pkgs.git}/bin/git clone https://github.com/tiborpilz/nixos $tempdir
            cp -r $tempdir/home/config/doom ~/.config/doom
        fi
        # .config/emacs/bin/doom sync
      '');
  };
}
