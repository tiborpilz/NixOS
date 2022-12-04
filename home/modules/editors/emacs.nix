{ lib, pkgs, inputs, ... }:
with lib;
let
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

  emacsPackage = emacsWithNativeComp;
in
{
  config = {
    programs.doom-emacs = rec {
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
      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
      };
      package = emacsPackage;

      extraConfig = ''
        (setenv "LSP_USE_PLISTS" "true")
      '';
    };

    home.packages = with pkgs; [
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

      # vue3 language server
      my."@volar/vue-language-server"

      # typescript language server
      nodePackages.typescript-language-server

      # Fonts
      emacs-all-the-icons-fonts
    ];
  };
}
