{ inputs, pkgs, lib, ... }:

let
  add-feature-flags = emacs: (emacs.override {
    withNativeCompilation = true;
    withXwidgets = true;
    withGTK3 = true;
  });

  emacsScript = emacsPkg: pkgs.writeShellScriptBin "emacs" ''
    #!/usr/bin/env bash
    . $HOME/.profile
    export LSP_USE_PLISTS true
    export WEBKIT_DISABLE_COMPOSITING_MODE true

    exec ${emacsPkg}/bin/emacs "$@"
  '';

  wrap = with pkgs; emacsPkg:
  let
    emacsScriptPath = emacsScript emacsPkg;
  in
    (symlinkJoin {
    name = "emacs";
    paths = [ emacsScriptPath emacsPkg ];
    nativeBuildInputs = [ makeBinaryWrapper ];
    meta = {
      platforms = emacsPkg.meta.platforms;
    };
    postBuild = ''
      if [ -d $out/Applications ]; then
        rm $out/Applications/Emacs.app/Contents/MacOS/Emacs
        wrapProgram $out/bin/emacs --set LSP_USE_PLISTS true --set WEBKIT_DISABLE_COMPOSITING_MODE 1
        cp $out/bin/emacs $out/Applications/Emacs.app/Contents/MacOS/Emacs
      else
        wrapProgram $out/bin/emacs --set LSP_USE_PLISTS true --set WEBKIT_DISABLE_COMPOSITING_MODE 1
      fi
    '';
  });

  patch-nul-char-bug = let
    json-nul-char-patch = (pkgs.fetchpatch {

    url = "https://github.com/emacs-mirror/emacs/commit/8b52d9f5f177ce76b9ebecadd70c6dbbf07a20c6.patch";
      hash = "sha256-/W9yateE9UZ9a8CUjavQw0X7TgxigYzBuOvtAXdEsSA=";
    });
    in emacs: emacs.overrideAttrs (old: { patches = old.patches or [] ++ [ json-nul-char-patch ]; });
  emacsPackages = lib.my.mapModules (toString ./.) (package: (import package) { inherit inputs pkgs lib; });
in
rec {
  emacsPatched = (patch-nul-char-bug pkgs.emacs);
  emacsXw = (add-feature-flags emacsPatched); #add-env (patch-nul-char-bug (add-feature-flags emacs));
  emacsXwWrapped = (wrap emacsXw);
  emacs29Wrapped = (wrap pkgs.emacs29);
  emacsGitXw = (add-feature-flags pkgs.emacs-git);
  emacsGitXwWrapped = (wrap emacsGitXw);
  emacsGitWrapped = (wrap pkgs.emacs-unstable);
} // emacsPackages.packages
