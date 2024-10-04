{ pkgs, ... }:
with pkgs;
stdenv.mkDerivation rec {
  name = "ansi2tmux";
  phases = "installPhase";
  src = ./ansi2tmux.sh;
  buildInputs = [ bash coreutils ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/ansi2tmux
    wrapProgram $out/bin/ansi2tmux \
      --prefix PATH : ${lib.makeBinPath [ bash ]}
  '';
}
