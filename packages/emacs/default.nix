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

  emacsBase =
    if pkgs.stdenv.hostPlatform.isDarwin
    then pkgs.unstable.emacs-git
    else pkgs.emacs-git;

  customEmacsPkg =
    (emacsBase.override {
      withNativeCompilation = true;
    }).overrideAttrs (old: {
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
      # :lang agda (+local loads agda2-mode via `agda-mode locate`)
      (agda.withPackages (p: [ p.standard-library ]))
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

  # Pre-compile Doom's packages to native code at build time, so they aren't
  # JIT-compiled on the first Emacs start after every rebuild. That runtime
  # compilation prints byte-compiler warnings (e.g. deprecated quoted
  # `condition-case` handlers in lsp-mode, evil-escape, etc.) to the daemon's
  # stderr, which `native-comp-async-report-warnings-errors' cannot suppress
  # (it only governs in-Emacs display, not the async child process's stderr).
  #
  # We discover the files to compile from the Doom Emacs load-path (the packages
  # live in a separate propagated store path, so `find ${doomEmacsRaw}` sees
  # nothing) and skip Emacs's own built-in lisp, which already ships .eln files.
  # Results go to share/emacs/native-lisp/, which Emacs adds to
  # native-comp-eln-load-path via NIX_PROFILES on startup. Byte-compiler
  # warnings stay in the build log instead of surfacing at runtime.
  doomEmacsElnFiles = pkgs.runCommand "doom-emacs-eln" { } ''
    export HOME=$TMPDIR
    mkdir -p $out/share/emacs/native-lisp
    ${doomEmacsRaw}/bin/emacs --batch \
      --eval "(let ((builtin (file-truename (file-name-directory (directory-file-name (file-name-directory (locate-library \"subr\")))))))
                (dolist (dir (copy-sequence load-path))
                  (when (and (file-directory-p dir)
                             (not (string-prefix-p builtin (file-truename dir))))
                    (dolist (f (directory-files dir t))
                      (when (and (string-suffix-p \".el\" f)
                                 (not (string-suffix-p \"-autoloads.el\" f))
                                 (not (string-suffix-p \"-pkg.el\" f)))
                        (princ f) (terpri))))))" \
      > $TMPDIR/doom-el-files.txt
    < $TMPDIR/doom-el-files.txt xargs -r -d '\n' -n 20 -P $NIX_BUILD_CORES \
      ${doomEmacsRaw}/bin/emacs --batch \
        --eval "(push \"$out/share/emacs/native-lisp\" native-comp-eln-load-path)" \
        --eval "(setq native-comp-async-report-warnings-errors 'silent)" \
        --eval "(progn (dolist (f command-line-args-left) (ignore-errors (native-compile f))) (setq command-line-args-left nil))" \
      || true
  '';

  # Merge the pre-compiled .eln files into the doom output tree.
  doomEmacsWithNativeComp = pkgs.symlinkJoin {
    name = "doom-emacs-native";
    paths = [ doomEmacsElnFiles doomEmacsRaw ];
  };

  doomEmacs = pkgs.runCommand "doom-emacs"
    {
      nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
      pname = "doom-emacs";
      version = doomEmacsRaw.version or "0";
      meta = (doomEmacsRaw.meta or { }) // {
        mainProgram = "doom-emacs";
      };
      passthru = (doomEmacsRaw.passthru or { }) // {
        unwrapped = doomEmacsRaw;
      };
    } ''
    mkdir -p $out/bin $out/share/applications $out/libexec/doom-emacs
    for entry in ${doomEmacsWithNativeComp}/bin/*; do
      name=$(basename "$entry")
      if [ "$name" = "emacs" ]; then
        ln -s "$entry" "$out/bin/doom-emacs"
      elif [ "$name" = "emacsclient" ]; then
        makeWrapper "$entry" "$out/bin/emacsclient" \
          --prefix PATH : "$out/libexec/doom-emacs"
      else
        ln -s "$entry" "$out/bin/$name"
      fi
    done
    ln -s "$out/bin/doom-emacs" "$out/libexec/doom-emacs/emacs"
    for entry in ${doomEmacsWithNativeComp}/*; do
      name=$(basename "$entry")
      if [ "$name" != "bin" ] && [ "$name" != "share" ]; then
        ln -s "$entry" "$out/$name"
      fi
    done
    for entry in ${doomEmacsWithNativeComp}/share/*; do
      name=$(basename "$entry")
      if [ "$name" != "applications" ]; then
        ln -s "$entry" "$out/share/$name"
      fi
    done

    for entry in ${doomEmacsWithNativeComp}/share/applications/*; do
      substitute "$entry" "$out/share/applications/$(basename "$entry")" \
        --replace-quiet "Exec=emacs " "Exec=$out/bin/doom-emacs " \
        --replace-quiet "TryExec=emacs" "TryExec=$out/bin/doom-emacs" \
        --replace-quiet "${customEmacsPkg}/bin/emacsclient" "$out/bin/emacsclient"
    done

    if grep -q "${customEmacsPkg}/bin/emacsclient" $out/share/applications/*; then
      echo "desktop entries still reference the unwrapped emacsclient" >&2
      exit 1
    fi
  '';
in
{
  emacs = pkgs.emacs;
  emacsWrapped = emacsWrapped;
  doom-emacs-config = doomConfig;
  doom-emacs = doomEmacs;
  doom-emacs-standalone = pkgs.doomEmacs doomArgs;
}
