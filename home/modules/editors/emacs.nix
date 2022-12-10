{ lib, pkgs, inputs, config, ... }:
with lib;
let
  cfg = config.modules.editors.emacs;
  mylib = import ../../../lib { inherit inputs lib pkgs; };
  # Otherwise, emacs can't handle some LSP control characters
  # patchedEmacs = pkgs.emacs.overrideAttrs (old: {
  #   patches = (old.patches or [ ]) ++ [
  #     (pkgs.fetchpatch {
  #       url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
  #       hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
  #     })
  #   ];
  #   postFixup = (old.postFixup or "") + "wrapProgram $out/bin/emacs --set LSP_USE_PLISTS 'true'";
  #   nativeComp = true;
  # });
  # emacsWithNativeComp = patchedEmacs.override {
  #   nativeComp = true;
  #   withXwidgets = true;
  #   withGTK3 = true;
  # };
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
      emacsPackage = pkgs.my.emacs;

      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
        emacs = pkgs.my.emacs;
        # self.straightBuild { pname = "lsp-mode"; }; #pkgs.my.lsp-mode;
      };
      # package = emacsPackage;

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

      # Fonts
      emacs-all-the-icons-fonts
      my.emmet-ls
    ];

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    # xdg.configFile."doom" = { source = ../../config/doom; recursive = true; };

    home.activation.installDoomEmacs =
      let activationScript = ''
          if [ ! -d ".config/emacs" ]; then
              ${pkgs.git}/bin/git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
          fi

          # if [ ! -d ".config/doom" ]; then
          #     tempdir=$(mktemp -d)
          #     ${pkgs.git}/bin/git clone https://github.com/tiborpilz/nixos $tempdir
          #     cp -r $tempdir/home/config/doom ~/.config/doom
          # fi
          # .config/emacs/bin/doom sync
        '';
      in (lib.hm.dag.entryAfter ["WriteBoundary"] (if cfg.useNix then "" else activationScript ));
  };
}
