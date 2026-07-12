{ lib
, stdenv
, racket
, makeWrapper
, pie-src
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "pie";
  version = "0.01-unstable-2021-07-07";

  src = pie-src;

  nativeBuildInputs = [ racket makeWrapper ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    collects="$out/share/pie-collects"
    mkdir -p "$collects/pie"
    cp -r ./. "$collects/pie/"
    # In case the source tree carried any stale build artifacts:
    find "$collects/pie" -type d -name compiled -exec rm -rf {} + 2>/dev/null || true

    # Compile to bytecode. Use throwaway HOME/addon dirs so nothing tries to
    # touch the real user profile during the build.
    export HOME="$TMPDIR/pie-build-home"
    export PLTADDONDIR="$TMPDIR/pie-build-addon"
    mkdir -p "$HOME" "$PLTADDONDIR"
    export PLTCOLLECTS="$collects:"
    raco setup --no-docs -l pie

    mkdir -p "$out/bin"

    # Pie-aware racket / raco: they behave exactly like upstream but can also
    # load `#lang pie`. Safe to put on PATH in a dev shell.
    # PLT_COMPILED_FILE_CHECK=exists tells Racket to trust the bytecode we
    # already compiled into the (read-only) store, instead of comparing
    # timestamps and trying to recompile into a path it can't write to.
    # Racket's own collections must be listed explicitly: an empty PLTCOLLECTS
    # entry ("$collects:") would splice them in, but makeWrapper strips
    # leading/trailing separators, so the empty entry never survives.
    makeWrapper ${racket}/bin/racket "$out/bin/racket" \
      --prefix PLTCOLLECTS : "$collects:${racket}/share/racket/collects" \
      --set-default PLT_COMPILED_FILE_CHECK exists
    makeWrapper ${racket}/bin/raco "$out/bin/raco" \
      --prefix PLTCOLLECTS : "$collects:${racket}/share/racket/collects" \
      --set-default PLT_COMPILED_FILE_CHECK exists

    # `pie` REPL
    makeWrapper "$out/bin/racket" "$out/bin/pie" \
      --add-flags "-l pie -i"

    runHook postInstall
  '';

  # prove the REPL loads and normalizes. PLTCOLLECTS from installPhase must be
  # unset, otherwise the check passes on the build env even with broken wrappers.
  doInstallCheck = true;
  installCheckPhase = ''
    unset PLTCOLLECTS
    echo '(the Nat (add1 (add1 zero)))' | "$out/bin/pie" | grep -q 'the Nat 2' \
      && echo "pie REPL OK"
    printf '#lang pie\n(the Nat 4)\n' > "$TMPDIR/check.pie"
    "$out/bin/racket" "$TMPDIR/check.pie" | grep -qx '(the Nat 4)' \
      && echo "racket wrapper OK"
  '';

  passthru = {
    collectionRoot = "${finalAttrs.finalPackage}/share/pie-collects";
  };

  meta = with lib; {
    description = "A little dependently-typed language to accompany *The Little Typer*";
    homepage = "https://github.com/the-little-typer/pie";
    license = licenses.agpl3Plus;
    mainProgram = "pie";
    platforms = platforms.unix;
  };
})
