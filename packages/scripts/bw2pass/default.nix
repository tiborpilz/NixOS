{ pkgs, ... }:
with pkgs;
stdenv.mkDerivation rec {
  name = "bw2pass";
  phases = "installPhase";
  src = ./bw2pass.sh;
  buildInputs = [ flock bash coreutils jq pkgs.unstable.bitwarden-cli ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/bw2pass
    wrapProgram $out/bin/bw2pass \
      --prefix PATH : ${lib.makeBinPath [ flock bash jq bitwarden-cli ]}
  '';
}
