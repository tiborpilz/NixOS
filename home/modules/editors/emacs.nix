# Took a lot of inspiration from hlissner's dotfiles repo
# although some tweaks were necessary, since this is a
# home-manager configuration, not a nixos configuration.

{ lib, pkgs, inputs, ... }:
with lib;
let
  repo = fetchGit {
    url = "https://github.com/tiborpilz/doom-emacs-config";
    ref = "feat/remove-recipe-packages";
    rev = inputs.doom-emacs-config.rev;
  };
in {
  config = {
    programs.doom-emacs = {
      enable = true;
      doomPrivateDir = repo;
      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
      };
    };
  };
}
# { lib, pkgs, inputs, ... }:
# with lib;
# let
#   patchedEmacs = pkgs.emacs.overrideAttrs (old: {
#     patches = (old.patches or []) ++ [
#       (pkgs.fetchpatch {
#         url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
#         hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
#       })
#     ];
#   });
#   emacsWithNativeComp = patchedEmacs.override {
#     nativeComp = true;
#   };
# in
# {
#   # config = mkIf cfg.enable {
#   config = {
#     # nixpkgs.overlays = [
#     #   inputs.emacs-overlay.overlay
#     #   # (self: super: {
#     #   #   emacs = super.emacs.override {
#     #   #     nativeComp = true;
#     #   #     withXwidgets = true;
#     #   #     withGTK3 = true;
#     #   #   };
#     #   # })
#     # ];

#     home.packages = with pkgs; [
#       ## Emacs
#       binutils # for native-comp
#       ## 28.2 + native-comp + xwidgets + gtk3
#       ((emacsPackagesFor emacsWithNativeComp).emacsWithPackages
#         (epkgs: [ epkgs.vterm ]))
#       # ((emacsPackagesFor emacs).emacsWithPackages
#       #   (epkgs: [ epkgs.vterm ]))
#       # (emacsWithPackagesFromUsePackage {
#       #   package = (pkgs.emacsGit.override {
#       #     withXwidgets = true;
#       #   });
#       # })
#       # ((emacsPackagesFor emacsNativeComp).emacsWithPackages
#       #   (epkgs: [ epkgs.vterm ]))

#       ## Doom dependencies
#       git
#       (ripgrep.override { withPCRE2 = true; })
#       gnutls

#       ## Optional dependencies
#       fd # for projectile
#       imagemagick # for image-dired
#       pinentry-emacs # in-emacs gnupg-prompts
#       zstd # for undo-fu-session/undo-tree compression

#       ## Module dependencies
#       # :checkers spell
#       (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

#       # :tools editorconfig
#       editorconfig-core-c

#       # :tools lookup & :lang org +roam
#       sqlite

#       # :lang latex & :lang org (late previews)
#       texlive.combined.scheme-medium

#       # alternative lsp server for nix
#       nil

#       # vue3 language server
#       my."@volar/vue-language-server"
#       # my."@volar/vue-typescript"
#       # my."@volar-plugins/vetur"

#       # typescript language server
#       nodePackages.typescript-language-server

#       # Fonts
#       emacs-all-the-icons-fonts
#     ];

#     home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

#     # modules.shell.zsh.rcFiles = [ "${config.xdg.configHome}/emacs/aliases.zsh" ];

#     home.activation.installDoomEmacs = lib.hm.dag.entryAfter ["WriteBoundary"] ''
#         if [ ! -d ".config/emacs" ]; then
#             git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs ".config/emacs"
#         fi

#         if [ ! -d ".config/doom" ]; then
#             git clone https://github.com/tiborpilz/doom-emacs-config ".config/doom"
#         fi
#         # .config/emacs/bin/doom sync
#       '';
#   };
# }
