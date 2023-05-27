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

  wrap = with pkgs; emacsPkg: (symlinkJoin {
    name = "emacs";
    paths = [ emacsPkg ];
    nativeBuildInputs = [ makeBinaryWrapper ];
    meta = emacsPkg.meta;
    postBuild = ''
      rm $out/Applications/Emacs.app/Contents/MacOS/Emacs
      cp $out/bin/emacs $out/Applications/Emacs.app/Contents/MacOS/Emacs
      echo "\$out ###"
      cat $out/bin/emacs
      echo "emacsScriptPath ###"
      cat ${emacsScriptPath.outPath}/bin/emacs
      # cp $out/bin/emacs $out/Applications/Emacs.app/Contents/MacOS/Emacs
      wrapProgram $out/Applications/Emacs.app/Contents/MacOS/Emacs --set LSP_USE_PLISTS true --set WEBKIT_DISABLE_COMPOSITING_MODE 1
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
  emacsStablePatched = (patch-nul-char-bug pkgs.emacs);
  emacsStableXw = (add-feature-flags emacsStablePatched);#add-env (patch-nul-char-bug (add-feature-flags emacs));
  emacsStableXwWrapped = (wrap emacsStableXw);
  emacsGitXw = (add-feature-flags pkgs.emacsGit);
  emacsGitXwWrapped = (wrap emacsGitXw);
  emacsGitWrapped = (wrap pkgs.emacsGit);
} // emacsPackages.packages
