{ pkgs, lib, ... }:

let
  emacsScript = emacsPkg: pkgs.writeShellScriptBin "emacs" ''
    #!/usr/bin/env bash
    export LSP_USE_PLISTS true
    export WEBKIT_DISABLE_COMPOSITING_MODE true
    export PATH="$PATH:$HOME/.nix-profile/bin"
    export PATH="$PATH:/nix/var/nix/profiles/default/bin"

    exec ${emacsPkg}/bin/emacs "$@"
  '';

  emacs-lsp-booster = pkgs.rustPlatform.buildRustPackage
    rec {
      pname = "emacs-lsp-booster";
      version = "0.2.1";

      cargoHash = "sha256-BR0IELLzm+9coaiLXQn+Rw6VLyiFEAk/nkO08qPwAac=";

      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = pname;
        rev = "v${version}";
        hash = "sha256:uP/xJfXQtk8oaG5Zk+dw+C2fVFdjpUZTDASFuj1+eYs=";
      };

      # The tests contain what are essentially benchmarks—it seems prudent not to
      # stress our users' computers in that way every time they build the package.
      doCheck = false;

      meta = with lib; {
        description = "Improve performance of Emacs LSP servers by converting JSON to bytecode";
        homepage = "https://github.com/${src.owner}/${pname}";
        changelog = "https://github.com/${src.owner}/${pname}/releases/tag/${version}";
        license = [ licenses.mit ];
        maintainers = [ ];
        mainProgram = "emacs-lsp-booster";
      };
    };

  customEmacs = pkgs.emacs.overrideAttrs (old: {
    # Add your custom compilation flags here
    # configureFlags = (old.configureFlags or [ ]) ++ [
    #   "--with-native-compilation"
    # ];
    # NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -O3";
  });

  wrap = with pkgs;
    emacsPkg:
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
  emacs = customEmacs;
  emacsWrapped = (wrap customEmacs);
  emacs-lsp-booster = emacs-lsp-booster;
}
