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

  # customEmacs =
  #   (pkgs.emacs-git.overrideAttrs (old: {
  #     stdenv = pkgs.ccacheStdenv;
  #     NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -O3";
  #   }));
  #
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

  emacsWrapped = wrap pkgs.emacs;

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
      nodePackages.mermaid-cli
      copilot-language-server
      emacs-lsp-booster
    ];
  };

  doomEmacs = (pkgs.emacsWithDoom doomArgs).overrideAttrs (old: {
    meta = (old.meta or { }) // {
      mainProgram = "emacs";
    };
  });
in
{
  emacs = pkgs.emacs;
  emacsWrapped = emacsWrapped;
  doom-emacs-config = doomConfig;
  doom-emacs = doomEmacs;
  doom-emacs-standalone = pkgs.doomEmacs doomArgs;
  emacs-lsp-booster = emacs-lsp-booster;
}
