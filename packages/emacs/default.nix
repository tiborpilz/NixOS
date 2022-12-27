{ inputs, pkgs, lib, ... }:

let
  add-feature-flags = emacs: (emacs.override {
    nativeComp = true;
    withXwidgets = true;
    withGTK3 = true;
  });

  add-env = emacs: key: value: (emacs.overrideAttrs (old: {
    postFixup = (old.postFixup or "") + "wrapProgram $out/bin/emacs --set ${key} ${value}";
  }));

  wrap = emacsPkg: (emacs.pkgs.symlinkJoin {
    name = "emacs";
    paths = [ emacsPkg ];
    nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
  wrap = with pkgs; emacsPkg: (symlinkJoin {
    name = "emacs";
    paths = [ emacsPkg ];
    nativeBuildInputs = [ makeBinaryWrapper ];
    postBuild = ''
      wrapProgram $out/bin/emacs --set LSP_USE_PLISTS true --set WEBKIT_DISABLE_COMPOSITING_MODE 1
    '';
  });

  add-plists-env = emacs: (add-env emacs "LSP_USE_PLISTS" "true");
  add-disable-webkit-composition-env = emacs: (add-env emacs "WEBKIT_DISABLE_COMPOSITING_MODE" "1");

  patch-nul-char-bug = let
    json-nul-char-patch = (pkgs.fetchpatch {
      url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
      hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
    });
    in emacs: emacs.overrideAttrs (old: { patches = old.patches or [] ++ [ json-nul-char-patch ]; });
  emacsPackages = lib.my.mapModules (toString ./.) (package: (import package) { inherit inputs pkgs lib; });
in
rec {
  emacs27Patched = (patch-nul-char-bug pkgs.emacs);
  emacs27Xw = (add-feature-flags emacs27Patched);#add-env (patch-nul-char-bug (add-feature-flags emacs));
  emacs27XwWrapped = (wrap emacs27Xw);
  emacsGitXw = (add-feature-flags pkgs.emacsGit);
  emacsGitXwWrapped = (wrap emacsGitXw);
} // emacsPackages.packages
