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

  emacsPackage = pkgs.emacs;
in
{
  config = {
    programs.doom-emacs = {
      enable = true;
      doomPrivateDir = repo;
      emacsPackagesOverlay = self: super: {
        copilot = pkgs.my.copilot;
      };
      package = emacsPackage;
    };

    # home.packages = [ emacsPackage ];
  };
}
