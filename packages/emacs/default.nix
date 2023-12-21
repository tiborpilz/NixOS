{ pkgs, ... }:

let
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
in
{
  emacsGitWrapped = (wrap pkgs.emacs-unstable);
}
