{ inputs, pkgs, lib, ... }:

with pkgs;
let
  add-feature-flags = emacs: (emacs.override {
    nativeComp = true;
    withXwidgets = true;
    withGTK3 = true;
  });
  add-env = emacs: key: value: (emacs.overrideAttrs (old: {
    postFixup = (old.postFixup or "") + "wrapProgram $out/bin/emacs --set ${key} ${value}";
  }));
  add-plists-env = emacs: (add-env emacs "LSP_USE_PLISTS" "true");

  patch-nul-char-bug = let
    json-nul-char-patch = (pkgs.fetchpatch {
      url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
      hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
    });
    in emacs: emacs.overrideAttrs (old: { patches = old.patches or [] ++ [ json-nul-char-patch ]; });
  emacsPackages = lib.my.mapModules (toString ./.) (package: (import package) { inherit inputs pkgs lib; });
in
{
  "emacs" = add-plists-env (patch-nul-char-bug (add-feature-flags emacs));
  "emacsGit" = add-plists-env (add-feature-flags emacsGit);
} // emacsPackages.packages
