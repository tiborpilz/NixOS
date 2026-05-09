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

  emacsBase =
    if pkgs.stdenv.hostPlatform.isDarwin
    then pkgs.unstable.emacs-git
    else pkgs.emacs-git;

  customEmacsPkg =
    (emacsBase.override {
      withNativeCompilation = true;
    }).overrideAttrs (old: {
      stdenv = pkgs.ccacheStdenv;
      NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -O3";
    });

  wrap = with pkgs;
    emacsPkg:
    let
      emacsScriptPath = emacsScript customEmacsPkg;
    in
    (symlinkJoin {
      name = "emacs";
      paths = [ emacsScriptPath customEmacsPkg ];
      nativeBuildInputs = [ makeBinaryWrapper ];
      meta = {
        platforms = customEmacsPkg.meta.platforms;
        mainProgram = customEmacsPkg.meta.mainProgram;
      };
      src = customEmacsPkg.src;
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

  emacsWrapped = wrap customEmacsPkg;

  treesitGrammars = with pkgs.tree-sitter.builtGrammars; {
    bash = tree-sitter-bash;
    css = tree-sitter-css;
    gleam = tree-sitter-gleam;
    go = tree-sitter-go;
    gomod = tree-sitter-gomod;
    gowork = tree-sitter-gowork;
    html = tree-sitter-html;
    javascript = tree-sitter-javascript;
    json = tree-sitter-json;
    nix = tree-sitter-nix;
    prisma = tree-sitter-prisma;
    python = tree-sitter-python;
    rust = tree-sitter-rust;
    svelte = tree-sitter-svelte;
    tsx = tree-sitter-tsx;
    typescript = tree-sitter-typescript;
    yaml = tree-sitter-yaml;
  };

  treesitGrammarPath = pkgs.runCommand "emacs-treesit-grammars" { } (
    ''
      mkdir -p $out
    '' + lib.concatStringsSep "\n" (lib.mapAttrsToList
      (language: grammar: ''
        ln -s ${grammar}/parser $out/libtree-sitter-${language}${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}
      '')
      treesitGrammars)
  );

  doomConfig = pkgs.stdenvNoCC.mkDerivation {
    pname = "doom-config";
    version = "dev";
    src = ../../home/config/doom;

    nativeBuildInputs = [ pkgs.perl ];

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out
      cp -R . $out
      chmod -R u+w $out

      perl -0pi -e 's/\(package! copilot\s+:recipe \(:host github\s+:repo "copilot-emacs\/copilot\.el"\s+:files \("\*\.el" "dist"\)\)\)/;; copilot is supplied by programs.doom-emacs.extraPackages./s' \
        $out/packages.el \
        $out/config.org

      substituteInPlace $out/packages.el $out/config.org \
        --replace-fail "(unpin! pcre2el)" ";; pcre2el remains pinned for nix-doom-emacs-unstraightened."

      printf '%s\n' \
        "" \
        "* Nix Package Integration" \
        "#+begin_src elisp" \
        ";;; Nix-provided tree-sitter grammars." \
        "(add-to-list 'treesit-extra-load-path \"${treesitGrammarPath}\")" \
        "#+end_src" \
        >> $out/config.org

      runHook postInstall
    '';
  };

  doomArgs = {
    doomDir = doomConfig;
    doomLocalDir = "~/.local/share/nix-doom";
    emacs = emacsWrapped;
    tangleArgs = "--all config.org";

    extraPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      (epkgs.melpaBuild {
        pname = "copilot";
        version = "0.2.0";
        src = pkgs.fetchFromGitHub {
          owner = "copilot-emacs";
          repo = "copilot.el";
          rev = "v0.2.0";
          sha256 = "sha256-hIA+qdWoOJI9/hqBUSHhmh+jjzDnPiZkIzszCPuQxd0=";
        };
        files = ''(:defaults "dist")'';
        packageRequires = with epkgs; [
          dash
          editorconfig
          f
          jsonrpc
          s
        ];
        propagatedUserEnvPkgs = [ pkgs.nodejs ];
      })
    ];

    extraBinPackages = with pkgs; [
      git
      ripgrep
      fd
      imagemagick
      pinentry-emacs
      zstd
      editorconfig-core-c
      sqlite
      gnuplot
      pandoc
      copilot-language-server
      emacs-lsp-booster
    ];
  };

  doomEmacsRaw = pkgs.emacsWithDoom doomArgs;

  # Pre-compile all Doom .el files to native code at build time.
  # The resulting .eln files land in share/emacs/native-lisp/, which Emacs
  # adds to native-comp-eln-load-path via NIX_PROFILES on startup.
  doomEmacsElnFiles = pkgs.runCommand "doom-emacs-eln" { } ''
    mkdir -p $out/share/emacs/native-lisp
    find ${doomEmacsRaw} -name '*.el' ! -name '*.dir-locals.el' -print0 | \
      xargs -0 -n 50 -P $NIX_BUILD_CORES ${doomEmacsRaw}/bin/emacs --batch \
        --eval "(setq native-compile-target-directory \"$out/share/emacs/native-lisp\")" \
        --eval "(setq native-comp-async-report-warnings-errors 'silent)" \
        -f batch-native-compile \
      || true
  '';

  # Merge the pre-compiled .eln files into the doom output tree.
  doomEmacsWithNativeComp = pkgs.symlinkJoin {
    name = "doom-emacs-native";
    paths = [ doomEmacsElnFiles doomEmacsRaw ];
  };

  doomEmacs = pkgs.runCommand "doom-emacs"
    {
      pname = "doom-emacs";
      version = doomEmacsRaw.version or "0";
      meta = (doomEmacsRaw.meta or { }) // {
        mainProgram = "doom-emacs";
      };
      passthru = (doomEmacsRaw.passthru or { }) // {
        unwrapped = doomEmacsRaw;
      };
    } ''
    mkdir -p $out/bin
    for entry in ${doomEmacsWithNativeComp}/bin/*; do
      name=$(basename "$entry")
      if [ "$name" = "emacs" ]; then
        ln -s "$entry" "$out/bin/doom-emacs"
      else
        ln -s "$entry" "$out/bin/$name"
      fi
    done
    for entry in ${doomEmacsWithNativeComp}/*; do
      name=$(basename "$entry")
      if [ "$name" != "bin" ]; then
        ln -s "$entry" "$out/$name"
      fi
    done
  '';
in
{
  emacs = pkgs.emacs;
  emacsWrapped = emacsWrapped;
  doom-emacs-config = doomConfig;
  doom-emacs = doomEmacs;
  doom-emacs-standalone = pkgs.doomEmacs doomArgs;
  emacs-lsp-booster = emacs-lsp-booster;
}
