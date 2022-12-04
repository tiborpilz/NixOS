{ version ? "dev", lib, stdenv, emacs }:

stdenv.mkDerivation {
  pname = "emacs-config";
  inherit version;
  src = lib.sourceByRegex ./. [ "config.org" "init.el" ];

  buildInputs = [emacs];

  buildPhase = ''
    cp $src/* .
    # Tangle org files
    emacs --batch -Q \
      -l org \
      config.org \
      -f org-babel-tangle
  '';

  dontUnpack = true;

  installPhase = ''
    install -D -t $out *.el
  '';
}
