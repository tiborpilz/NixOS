{ pkgs, ... }:

let
  emacsScript = emacsPkg: pkgs.writeShellScriptBin "emacs" ''
    #!/usr/bin/env bash
    export LSP_USE_PLISTS true
    export WEBKIT_DISABLE_COMPOSITING_MODE true
    export PATH="$PATH:$HOME/.nix-profile/bin"
    export PATH="$PATH:/nix/var/nix/profiles/default/bin"

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
      mainProgram = emacsPkg.meta.mainProgram;
    };
    src = emacsPkg.src;
    postBuild = ''
    wrapProgram $out/bin/emacs \
      --set LSP_USE_PLISTS true \
      --set WEBKIT_DISABLE_COMPOSITING_MODE 1

      # Only applicable on Darwin
      if [ -d $out/Applications ]; then
        rm $out/Applications/Emacs.app/Contents/MacOS/Emacs
        cp $out/bin/emacs $out/Applications/Emacs.app/Contents/MacOS/Emacs
      fi
    '';
  });
in
{
  emacsGitWrapped = (wrap pkgs.emacs-unstable);
}
