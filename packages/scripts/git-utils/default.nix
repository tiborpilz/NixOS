{ pkgs, ... }:
with pkgs;
stdenv.mkDerivation {
  name = "git-utils";
  phases = "installPhase";
  src = ./.;
  buildInputs = [ git fzf ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
      mkdir -p $out/bin
      cp $src/gch.sh $out/bin/gch
      wrapProgram $out/bin/gch \
        --prefix PATH : ${lib.makeBinPath [ git fzf ]}
    '';
}
